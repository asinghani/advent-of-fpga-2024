open! Core
open! Hardcaml
open! Signal

module Make (Design : Ulx3s.Design) = struct
  module I = struct
    type 'a t =
      { clock25 : 'a
      ; btn : 'a [@bits 7]
      ; ftdi_txd : 'a
      ; ftdi_nrts : 'a
      ; ftdi_ndtr : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { led : 'a [@bits 8]
      ; ftdi_rxd : 'a
      }
    [@@deriving hardcaml]
  end

  module Byte_with_valid = Uart.Byte_with_valid

  module Uart = Uart.Make (struct
      let baud = 115200
      let clock_freq_hz = Ulx3s.Clock_freq.to_int_hz Design.clock_freq
      let rx_fifo_depth = Design.uart_fifo_depth
      let tx_fifo_depth = 128
    end)

  let create scope (i : _ I.t) : _ O.t =
    let clock =
      match Design.clock_freq with
      | Clock_25mhz -> i.clock25
    in
    (* Synchronize inputs and wrap the UART for the user design to use *)
    let sync_input x = pipeline (Reg_spec.create ~clock ()) ~n:3 x in
    let btn = sync_input i.btn in
    let clear_btn = ~:(btn.:(0)) in
    let uart_rts = sync_input ~:(i.ftdi_nrts) in
    let uart_dtr = sync_input ~:(i.ftdi_ndtr) in
    let ftdi_rx = sync_input i.ftdi_txd in
    (* Use DTR as an extra clear input *)
    let clear = clear_btn |: uart_dtr in
    let uart_rx_io =
      let module M = Structural_inst.Make (Uart.Rx) in
      M.create scope
    in
    uart_rx_io.i.clock <== clock;
    uart_rx_io.i.clear <== clear;
    uart_rx_io.i.rx <== ftdi_rx;
    let uart_tx_io =
      let module M = Structural_inst.Make (Uart.Tx) in
      M.create scope
    in
    uart_tx_io.i.clock <== clock;
    uart_tx_io.i.clear <== clear;
    let%tydi { leds; uart_tx; uart_rx_ready } =
      Design.hierarchical
        scope
        { clock
        ; clear
        ; buttons =
            { up = btn.:(3)
            ; down = btn.:(4)
            ; left = btn.:(5)
            ; right = btn.:(6)
            ; fire1 = btn.:(1)
            ; fire2 = btn.:(2)
            }
        ; uart_tx_ready = uart_tx_io.o.ready
        ; uart_rx = uart_rx_io.o.byte_out
        ; uart_rts
        ; uart_rx_overflow = uart_rx_io.o.fifo_overflow
        }
    in
    uart_rx_io.i.ready <== uart_rx_ready;
    Byte_with_valid.Of_signal.assign uart_tx_io.i.byte_in uart_tx;
    { led = leds; ftdi_rxd = uart_tx_io.o.tx }
  ;;
end

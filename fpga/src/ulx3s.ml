open! Core
open! Hardcaml

module Buttons = struct
  type 'a t =
    { up : 'a
    ; down : 'a
    ; left : 'a
    ; right : 'a
    ; fire1 : 'a
    ; fire2 : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; buttons : 'a Buttons.t
    ; uart_rx : 'a Uart.Byte_with_valid.t
    ; uart_tx_ready : 'a
    ; uart_rts : 'a
    ; uart_rx_overflow : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module O = struct
  type 'a t =
    { leds : 'a [@bits 8]
    ; uart_tx : 'a Uart.Byte_with_valid.t
    ; uart_rx_ready : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module Clock_freq = struct
  type t = Clock_25mhz

  let to_int_hz = function
    | Clock_25mhz -> 25000000
  ;;
end

module type Design = sig
  val clock_freq : Clock_freq.t
  val uart_fifo_depth : int
  val extra_synth_args : string list
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

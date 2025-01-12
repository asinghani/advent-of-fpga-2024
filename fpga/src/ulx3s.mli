open! Core
open! Hardcaml

module Buttons : sig
  type 'a t =
    { up : 'a
    ; down : 'a
    ; left : 'a
    ; right : 'a
    ; fire1 : 'a
    ; fire2 : 'a
    }
  [@@deriving hardcaml]
end

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; buttons : 'a Buttons.t
    ; uart_rx : 'a Uart.Byte_with_valid.t
    ; uart_tx_ready : 'a (** Backpressure from the UART TX FIFO *)
    ; uart_rts : 'a
    (** The RTS signal communicated by the UART, useful as an extra signaling
        bit to allow maintaining the simplicity of the 8-bit UART. *)
    ; uart_rx_overflow : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { leds : 'a
    ; uart_tx : 'a Uart.Byte_with_valid.t
    ; uart_rx_ready : 'a (** Allow user design to backpressure the UART RX FIFO *)
    }
  [@@deriving hardcaml]
end

module Clock_freq : sig
  type t = Clock_25mhz

  val to_int_hz : t -> int
end

module type Design = sig
  val clock_freq : Clock_freq.t
  val uart_fifo_depth : int
  val extra_synth_args : string list
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

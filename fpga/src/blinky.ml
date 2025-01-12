open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

let create scope ({ clock; clear; buttons; uart_rx; uart_rts; _ } : _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let count_to = Ulx3s.Clock_freq.to_int_hz clock_freq / 2 in
  let count =
    reg_fb spec ~width:(num_bits_to_represent count_to) ~f:(fun x ->
      mux2 (x ==:. count_to - 1) (of_int ~width:(width x) 0) (x +:. 1))
  in
  let counter ~width cond = reg_fb spec ~width ~enable:cond ~f:(fun x -> x +:. 1) in
  let uart_rts_rising = uart_rts &: ~:(reg spec uart_rts) in
  { leds =
      concat_msb [ counter ~width:4 uart_rts_rising; counter ~width:4 (count ==:. 0) ]
  ; uart_tx = Uart.Byte_with_valid.Of_signal.of_int 0
  ; uart_rx_ready = gnd
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

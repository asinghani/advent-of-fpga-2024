open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

(* UART loopback design with a counter and simulated backpressure for testing *)
let create scope ({ clock; clear; buttons; uart_rx; uart_rts; _ } : _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let count_to = Ulx3s.Clock_freq.to_int_hz clock_freq / 2 in
  let count =
    reg_fb spec ~width:(num_bits_to_represent count_to) ~f:(fun x ->
      mux2 (x ==:. count_to - 1) (of_int ~width:(width x) 0) (x +:. 1))
  in
  let counter = reg_fb spec ~width:8 ~enable:(count ==:. 0) ~f:(fun x -> x +:. 1) in
  { leds = counter
  ; uart_tx =
      { valid = uart_rts &: uart_rx.valid |: (count ==:. 0)
      ; value = mux2 (uart_rts &: uart_rx.valid) uart_rx.value counter
      }
  ; uart_rx_ready = uart_rts
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

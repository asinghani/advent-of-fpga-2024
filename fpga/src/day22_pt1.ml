open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 4096
let extra_synth_args = []
let accumulator_width = 24
let num_steps = 2000

module States = struct
  type t =
    | Wait_for_new_seed
    | Step
    | Accumulate
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let mix a b =
  let width = max (width a) (width b) in
  uresize ~width a ^: uresize ~width b
;;

let prune x = uresize ~width:accumulator_width x

(* The actual PRNG function *)
let xorshift x =
  let x = mix (sll ~by:6 x) x |> prune in
  let x = mix (srl ~by:5 x) x |> prune in
  let x = mix (sll ~by:11 x) x |> prune in
  x
;;

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in
  let sm = State_machine.create (module States) spec in
  ignore (sm.current -- "state");
  let%hw_var count = Variable.reg spec ~width:(num_bits_to_represent num_steps) in
  let%hw_var value = Variable.reg spec ~width:accumulator_width in
  let%hw_var result = Variable.reg spec ~width:40 in
  compile
    [ sm.switch
        [ ( Wait_for_new_seed
          , [ when_
                word_in.valid
                [ if_ (word_in.value ==:. 0) [ sm.set_next Done ]
                  @@ else_
                       [ value <-- uresize ~width:(width value.value) word_in.value
                       ; count <--. 0
                       ; sm.set_next Step
                       ]
                ]
            ] )
        ; ( Step
          , [ incr count
            ; when_ (count.value ==:. num_steps - 1) [ sm.set_next Accumulate ]
            ; value <-- xorshift value.value
            ] )
        ; ( Accumulate
          , [ result
              <-- uresize ~width:(width result.value) Uop.(result.value +: value.value)
            ; sm.set_next Wait_for_new_seed
            ] )
        ; Done, []
        ]
    ];
  let%hw done_ = sm.is Done in
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 result.value; valid = done_ }
      ; part2 = { value = zero 60; valid = done_ }
      }
  in
  { leds = concat_lsb [ ~:clear; uart_rx_overflow; zero 6 ]
  ; uart_tx = byte_out
  ; uart_rx_ready = vdd
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 4096
let extra_synth_args = []
let step_bits = 8
let pt2_offset = 10000000000000
let goal_bits = num_bits_to_represent pt2_offset + 1
let accumulator_bits = 48
let div_width = 3 + goal_bits + step_bits

module Divider = Hardcaml_circuits.Divider.Make (struct
    let width = div_width
    let signedness = Signedness.Signed
    let architecture = Hardcaml_circuits.Divider.Architecture.Iterative
  end)

module States = struct
  type t =
    | Load_xa
    | Load_ya
    | Load_xb
    | Load_yb
    | Load_xgoal
    | Load_ygoal
    | Cross_multiply_pt1
    | Divide_pt1
    | Accumulate_pt1
    | Offset_for_pt2
    | Cross_multiply_pt2
    | Divide_pt2
    | Accumulate_pt2
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

(* TODO: figure out why this works *)
let fixup_div
  ~spec
  ({ quotient; remainder; valid } : _ Divider.O.t)
  ~numerator
  ~denominator
  : _ Divider.O.t
  =
  let invalid =
    Uop.(reg spec (quotient *: denominator) <>: reg spec (numerator -: remainder))
  in
  { quotient = mux2 invalid (quotient +:. 1) quotient
  ; remainder
  ; valid = valid &: reg spec valid
  }
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
  let%hw_var result_pt1 = Variable.reg spec ~width:accumulator_bits in
  let%hw_var result_pt2 = Variable.reg spec ~width:accumulator_bits in
  let%hw_var xa = Variable.reg spec ~width:step_bits in
  let%hw_var ya = Variable.reg spec ~width:step_bits in
  let%hw_var xb = Variable.reg spec ~width:step_bits in
  let%hw_var yb = Variable.reg spec ~width:step_bits in
  let%hw_var xgoal = Variable.reg spec ~width:goal_bits in
  let%hw_var ygoal = Variable.reg spec ~width:goal_bits in
  let%hw_var start_divide = Variable.wire ~default:gnd in
  let after_n_clocks ~n logic =
    let counter = Variable.reg ~width:(num_bits_to_represent n) spec in
    proc
      [ counter <-- counter.value +:. 1
      ; when_ (counter.value ==:. n - 1) [ counter <--. 0; proc logic ]
      ]
  in
  (* We want to compute the following, which is the closed-form solution:
     d = xa*yb - xb*ya
     a = (yb*xp - xb*yp) / d
     b = (xa*yp - ya*xp) / d
  *)
  (* Implement the numerator / denominator calculation as a pipeline, and just
     wait a few cycles in the state machine for it to propagate *)
  let%hw denominator =
    reg spec Sop.(reg spec (xa.value *: yb.value) -: reg spec (xb.value *: ya.value))
  in
  let%hw numerator_a =
    reg
      spec
      Sop.(reg spec (yb.value *: xgoal.value) -: reg spec (xb.value *: ygoal.value))
  in
  let%hw numerator_b =
    reg
      spec
      Sop.(reg spec (xa.value *: ygoal.value) -: reg spec (ya.value *: xgoal.value))
  in
  (* Normalize the signs *)
  let%hw denominator_is_negative = msb denominator in
  let%hw denominator = mux2 denominator_is_negative (negate denominator) denominator in
  let%hw numerator_a = mux2 denominator_is_negative (negate numerator_a) numerator_a in
  let%hw numerator_b = mux2 denominator_is_negative (negate numerator_b) numerator_b in
  let div_a =
    Divider.hierarchical
      ~name:"div_a"
      scope
      { clock
      ; clear
      ; numerator = sresize ~width:div_width numerator_a
      ; denominator = sresize ~width:div_width denominator
      ; start = start_divide.value
      }
  in
  let div_a = fixup_div ~spec ~numerator:numerator_a ~denominator div_a in
  let div_b =
    Divider.hierarchical
      ~name:"div_b"
      scope
      { clock
      ; clear
      ; numerator = sresize ~width:div_width numerator_b
      ; denominator = sresize ~width:div_width denominator
      ; start = start_divide.value
      }
  in
  let div_b = fixup_div ~spec ~numerator:numerator_b ~denominator div_b in
  compile
    [ sm.switch
        [ ( Load_xa
          , [ when_
                word_in.valid
                [ if_ (word_in.value ==:. 0) [ sm.set_next Done ]
                  @@ else_
                       [ xa <-- uresize ~width:step_bits word_in.value
                       ; sm.set_next Load_ya
                       ]
                ]
            ] )
        ; ( Load_ya
          , [ when_
                word_in.valid
                [ ya <-- uresize ~width:step_bits word_in.value; sm.set_next Load_xb ]
            ] )
        ; ( Load_xb
          , [ when_
                word_in.valid
                [ xb <-- uresize ~width:step_bits word_in.value; sm.set_next Load_yb ]
            ] )
        ; ( Load_yb
          , [ when_
                word_in.valid
                [ yb <-- uresize ~width:step_bits word_in.value; sm.set_next Load_xgoal ]
            ] )
        ; ( Load_xgoal
          , [ when_
                word_in.valid
                [ xgoal <-- uresize ~width:goal_bits word_in.value
                ; sm.set_next Load_ygoal
                ]
            ] )
        ; ( Load_ygoal
          , [ when_
                word_in.valid
                [ ygoal <-- uresize ~width:goal_bits word_in.value
                ; sm.set_next Cross_multiply_pt1
                ]
            ] )
          (* The cross-multiplying logic is implemented above as a pipeline, so just step forward a few cycles here *)
        ; ( Cross_multiply_pt1
          , [ after_n_clocks ~n:10 [ sm.set_next Divide_pt1; start_divide <-- vdd ] ] )
        ; ( Divide_pt1
          , [ when_ (div_a.valid &: div_b.valid) [ sm.set_next Accumulate_pt1 ] ] )
        ; ( Accumulate_pt1
          , [ when_
                (div_a.remainder
                 ==:. 0
                 &: (div_b.remainder ==:. 0)
                 &: ~:(msb div_a.quotient)
                 &: ~:(msb div_b.quotient))
                [ result_pt1
                  <-- uresize
                        ~width:(width result_pt1.value)
                        Uop.(
                          result_pt1.value
                          +: ((div_a.quotient *: of_int ~width:2 3) +: div_b.quotient))
                ]
            ; sm.set_next Offset_for_pt2
            ] )
        ; ( Offset_for_pt2
          , [ xgoal <-- xgoal.value +: of_int ~width:(width xgoal.value) pt2_offset
            ; ygoal <-- ygoal.value +: of_int ~width:(width ygoal.value) pt2_offset
            ; sm.set_next Cross_multiply_pt2
            ] )
        ; ( Cross_multiply_pt2
          , [ after_n_clocks ~n:15 [ sm.set_next Divide_pt2; start_divide <-- vdd ] ] )
        ; ( Divide_pt2
          , [ when_ (div_a.valid &: div_b.valid) [ sm.set_next Accumulate_pt2 ] ] )
        ; ( Accumulate_pt2
          , [ when_
                (div_a.remainder
                 ==:. 0
                 &: (div_b.remainder ==:. 0)
                 &: ~:(msb div_a.quotient)
                 &: ~:(msb div_b.quotient))
                [ result_pt2
                  <-- uresize
                        ~width:(width result_pt2.value)
                        Uop.(
                          result_pt2.value
                          +: ((div_a.quotient *: of_int ~width:2 3) +: div_b.quotient))
                ]
            ; sm.set_next Load_xa
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
      ; part1 = { value = uresize ~width:60 result_pt1.value; valid = done_ }
      ; part2 = { value = uresize ~width:60 result_pt2.value; valid = done_ }
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

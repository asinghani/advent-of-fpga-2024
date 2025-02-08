open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 65536
let extra_synth_args = [ "-dff" ]
let accum_bits = 64
let operand_bits = 10
let max_seq_len = 13

(* Pack the operand together with the number of digits in the operand, to
   simplify the "concatenate" operation later *)
module Operand = struct
  type 'a t =
    { value : 'a [@bits operand_bits]
    ; digits : 'a [@bits 2]
    }
  [@@deriving hardcaml ~rtlmangle:"$"]

  (* Compute the number of digits in the operand and pack it together with the
     value itself *)
  let make value =
    let value = uresize ~width:operand_bits value in
    let digits =
      mux2 (value >=:. 100) (of_int ~width:2 3)
      @@ mux2 (value >=:. 10) (of_int ~width:2 2)
      @@ of_int ~width:2 1
    in
    { value; digits }
  ;;

  let none = Of_signal.of_int 0
end

(* Enum for the allowed operators, along with Nop which is used to pad the end
   of a sequence *)
module Operator = struct
  module Cases = struct
    type t =
      | Add
      | Mul
      | Cat
      | Nop
    [@@deriving sexp_of, compare, enumerate]
  end

  module Enum = Hardcaml.Enum.Make_enums (Cases)
  include Enum.Binary
end

module States = struct
  type t =
    | Load_sequence
    | Dispatch_part1
    | Wait_part1_finish
    | Dispatch_part2
    | Wait_part2_finish
    | Accumulate
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

(* Each stage of the pipeline contains the current accumulator, the goal value,
   and the remaining operands and operators *)
module Pipeline_state = struct
  type 'a t =
    { valid : 'a
    ; accum : 'a [@bits accum_bits]
    ; goal : 'a [@bits accum_bits]
    ; operands : 'a Operand.t list [@length max_seq_len - 1]
    ; operators : 'a Operator.t list [@length max_seq_len - 1]
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

let does_result_overflow result =
  let result = uresize ~width:(Int.max (width result) (accum_bits + 1)) result in
  let overflow, value = split_in_half_lsb ~lsbs:accum_bits result in
  let overflow = overflow <>:. 0 in
  value, overflow
;;

let pow10 x =
  let max_input = (1 lsl width x) - 1 in
  let max_width = num_bits_to_represent (Int.pow 10 max_input) in
  mux_init x (1 lsl width x) ~f:(fun x -> of_unsigned_int ~width:max_width (Int.pow 10 x))
;;

(* Make a single pipeline stage which takes a Pipeline_state.t as input and
   returns the next Pipeline_state.t as output. *)
let make_pipeline_stage ~spec scope (input : _ Pipeline_state.t) =
  let naming_op = Scope.naming scope in
  let%tydi { valid; accum; goal; operands; operators } =
    Pipeline_state.Of_signal.apply_names ~naming_op ~prefix:"input$" input
  in
  let operand = List.hd_exn operands in
  (* If the operand is empty, the operation should always be a Nop *)
  let operator =
    Operator.Of_signal.(
      mux2 (operand.digits ==:. 0) (of_enum Nop) (List.hd_exn operators))
  in
  let accum_pipeline_cycles = 1 in
  let accum_mux =
    [ (Operator.Cases.Add, Uop.(accum +: operand.value))
    ; (Mul, Uop.(accum *: operand.value))
    ; (Cat, Uop.((accum *: pow10 operand.digits) +: operand.value))
    ; Nop, accum
    ]
    |> List.map ~f:(Tuple2.map_snd ~f:(reg spec))
    |> List.map ~f:(Tuple2.map_snd ~f:does_result_overflow)
  in
  (* Mux the results for each operator based on the currently selected operator *)
  let accum_new =
    Operator.Of_signal.match_
      operator
      (accum_mux |> List.map ~f:(Tuple2.map_snd ~f:Tuple2.get1))
  in
  let overflow =
    Operator.Of_signal.match_
      operator
      (accum_mux |> List.map ~f:(Tuple2.map_snd ~f:Tuple2.get2))
  in
  (* Line up the pipeline with the cycles taken to calculate the operation result *)
  { Pipeline_state.valid = pipeline spec ~n:accum_pipeline_cycles valid &: ~:overflow
  ; accum = accum_new
  ; goal =
      goal |> pipeline spec ~n:accum_pipeline_cycles
      (* For the next stage, output the remaining operators and operands, dropping
         the one we just used *)
  ; operands =
      List.tl_exn operands @ [ Operand.none ]
      |> List.map ~f:(Operand.Of_signal.pipeline spec ~n:accum_pipeline_cycles)
  ; operators =
      List.tl_exn operators @ [ Operator.Of_signal.of_enum Nop ]
      |> List.map ~f:(Operator.Of_signal.pipeline spec ~n:accum_pipeline_cycles)
      (* Output the remaining operands and operators, dropping the one we just used *)
  }
  |> Pipeline_state.Of_signal.apply_names ~naming_op ~prefix:"output$"
  |> Pipeline_state.Of_signal.reg spec
;;

(* Recursively build the full pipeline *)
let rec make_pipeline ?(n = max_seq_len) ~spec scope input =
  match n with
  | 0 -> input
  | _ ->
    let subscope = Scope.sub_scope scope [%string "stage%{n#Int}"] in
    make_pipeline ~n:(n - 1) ~spec scope (make_pipeline_stage ~spec subscope input)
;;

(* Base-N counter *)
let rec n_ary_counter ~n values =
  if List.is_empty values
  then []
  else (
    let first = List.hd_exn values in
    let rest =
      match List.tl values with
      | Some x -> x
      | None -> []
    in
    let value_plus_one = first +:. 1 in
    let carry = value_plus_one >=:. n in
    let with_carry = zero (width first) :: n_ary_counter ~n rest in
    let without_carry = value_plus_one :: rest in
    List.map2_exn with_carry without_carry ~f:(mux2 carry))
;;

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let ( -- ) = Scope.naming scope in
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in
  ignore (sm.current -- "state");
  let uart_rx_ready = sm.is Load_sequence in
  let input = Util.shift_in ~clock ~clear ~n:8 ~ready:uart_rx_ready uart_rx in
  let after_n_clocks ~n logic =
    let counter = Variable.reg ~width:(num_bits_to_represent n) spec in
    proc
      [ counter <-- counter.value +:. 1
      ; when_ (counter.value ==:. n - 1) [ counter <--. 0; proc logic ]
      ]
  in
  let%hw_var seq_len = Variable.reg ~width:(num_bits_to_represent max_seq_len) spec in
  let%hw_var goal = Variable.reg ~width:accum_bits spec in
  let%hw_var operator_incr_binary = Variable.wire ~default:gnd in
  let%hw_var operator_incr_ternary = Variable.wire ~default:gnd in
  let%hw_var operator_reset = Variable.wire ~default:gnd in
  let operator_counter =
    List.init max_seq_len ~f:(fun _ -> wire Operator.sum_of_port_widths)
    |> List.mapi ~f:(fun i x -> x -- [%string "operator_counter%{i#Int}"])
  in
  let operator_counter_next_binary =
    n_ary_counter ~n:2 operator_counter
    |> List.mapi ~f:(fun i x -> x -- [%string "operator_counter_next_binary%{i#Int}"])
  in
  let operator_counter_next_ternary =
    n_ary_counter ~n:3 operator_counter
    |> List.mapi ~f:(fun i x -> x -- [%string "operator_counter_next_ternary%{i#Int}"])
  in
  let _ =
    List.map3_exn
      operator_counter
      operator_counter_next_binary
      operator_counter_next_ternary
      ~f:(fun w b t ->
        w
        <== reg
              spec
              ~clear:operator_reset.value
              (mux2 operator_incr_ternary.value t
               @@ mux2 operator_incr_binary.value b
               @@ w))
  in
  let%hw operator_counter_overflow = mux seq_len.value operator_counter <>:. 0 in
  let%hw_var part1_match = Variable.reg ~width:1 spec in
  let%hw_var part2_match = Variable.reg ~width:1 spec in
  let%hw_var part1_accum = Variable.reg ~width:60 spec in
  let%hw_var part2_accum = Variable.reg ~width:60 spec in
  let%hw_var dispatch_valid = Variable.wire ~default:gnd in
  let operands = List.init max_seq_len ~f:(fun _ -> Operand.Of_always.reg spec) in
  let result =
    make_pipeline
      ~spec
      scope
      { valid = dispatch_valid.value
      ; accum = (List.hd_exn operands).value.value
      ; goal = goal.value
      ; operands = List.tl_exn operands |> List.map ~f:Operand.Of_always.value
      ; operators =
          (* The operator counter is one symbol longer than it needs to be, to
             make it easier to detect overflows *)
          List.drop_last_exn operator_counter |> List.map ~f:Operator.Of_signal.of_raw
      }
  in
  let%hw result_match = result.valid &: (result.accum ==: result.goal) in
  compile
    [ sm.switch
        [ ( Load_sequence
          , [ when_
                input.valid
                [ if_
                    (input.value ==:. 0)
                    [ if_ (seq_len.value ==:. 0) [ sm.set_next Done ]
                      @@ else_ [ decr ~by:2 seq_len; sm.set_next Dispatch_part1 ]
                    ]
                  @@ else_
                       [ incr seq_len
                       ; if_ (seq_len.value ==:. 0) [ goal <-- input.value ]
                         @@ else_
                              [ proc
                                  (List.mapi operands ~f:(fun i o ->
                                     when_
                                       (seq_len.value ==:. i + 1)
                                       [ Operand.Of_always.assign
                                           o
                                           (Operand.make input.value)
                                       ]))
                              ]
                       ]
                ]
            ; operator_reset <-- vdd
            ] )
        ; ( Dispatch_part1
          , [ dispatch_valid <-- vdd
            ; operator_incr_binary <-- vdd
            ; when_ operator_counter_overflow [ sm.set_next Wait_part1_finish ]
            ; when_ result_match [ part1_match <-- vdd ]
            ] )
        ; ( Wait_part1_finish
          , [ after_n_clocks ~n:100 [ sm.set_next Dispatch_part2 ]
            ; when_ result_match [ part1_match <-- vdd ]
            ; operator_reset <-- vdd
            ] )
        ; ( Dispatch_part2
          , [ dispatch_valid <-- vdd
            ; operator_incr_ternary <-- vdd
            ; when_ operator_counter_overflow [ sm.set_next Wait_part2_finish ]
            ; when_ result_match [ part2_match <-- vdd ]
            ] )
        ; ( Wait_part2_finish
          , [ after_n_clocks ~n:100 [ sm.set_next Accumulate ]
            ; when_ result_match [ part2_match <-- vdd ]
            ; operator_reset <-- vdd
            ] )
        ; ( Accumulate
          , [ when_
                part1_match.value
                [ part1_accum
                  <-- part1_accum.value
                      +: uresize ~width:(width part1_accum.value) goal.value
                ]
            ; when_
                part2_match.value
                [ part2_accum
                  <-- part2_accum.value
                      +: uresize ~width:(width part2_accum.value) goal.value
                ]
            ; sm.set_next Load_sequence
            ; seq_len <--. 0
            ; part1_match <-- gnd
            ; part2_match <-- gnd
            ; proc (List.map operands ~f:(fun o -> o.digits <--. 0))
            ; operator_reset <-- vdd
            ] )
        ; Done, []
        ]
    ];
  let done_ = sm.is Done in
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 part1_accum.value; valid = done_ }
      ; part2 = { value = uresize ~width:60 part2_accum.value; valid = done_ }
      }
  in
  { leds = concat_lsb [ ~:clear; uart_rx_overflow; zero 6 ]
  ; uart_tx = byte_out
  ; uart_rx_ready
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

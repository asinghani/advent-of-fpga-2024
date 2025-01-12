(* not working yet *)
open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

(* 19^4 = 130321, hence use 17 bits for 131072 rows *)
let total_address_bits = 12
let pt2_accumulator_bits = 16

module Single_result_ram = struct
  (* Smallest BRAM = 18 x 1024 *)
  let ram_address_bits = 10
  let ram_index_bits = total_address_bits - ram_address_bits
  let ram_depth = 1 lsl ram_address_bits
  let accumulate_cycles = ram_depth + 5

  module Ram = Loadable_pseudo_dual_port_ram.Make (struct
      let width = pt2_accumulator_bits + 1
      let depth = ram_depth
      let zero_on_startup = true
      let num_ports = 2
    end)

  module States = struct
    type t =
      | Idle
      | Ram_read1
      | Ram_read2
      | Ram_write
      | Accumulate_maximum
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; address : 'a [@bits total_address_bits]
      ; value_to_add : 'a [@bits 4]
      ; value_valid : 'a
      ; end_of_sequence : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { max_value : 'a [@bits pt2_accumulator_bits] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ?bel
    ~ram_index
    scope
    ({ clock; clear; address; value_to_add; value_valid; end_of_sequence } : _ I.t)
    : _ O.t
    =
    assert (ram_index >= 0 && ram_index < 1 lsl ram_index_bits);
    let open Always in
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = State_machine.create (module States) spec in
    let%hw_var value_in_reg = Variable.reg spec ~width:4 in
    let%hw_var ram_address0 = Variable.reg spec ~width:ram_address_bits in
    let%hw_var ram_write0_en = Variable.reg spec ~width:1 in
    let%hw_var ram_write0_value = Variable.reg spec ~width:(pt2_accumulator_bits + 1) in
    let%hw_var accum_address = Variable.reg spec ~width:ram_address_bits in
    let%hw_var accum_valid = Variable.reg spec ~width:1 in
    let%hw_var max_value_clear = Variable.wire ~default:gnd in
    let index, address = split_in_half_lsb ~lsbs:ram_address_bits address in
    let match_ = index ==:. ram_index in
    let ram_read0 = wire (pt2_accumulator_bits + 1) in
    let max_value =
      reg_fb spec ~clear:max_value_clear.value ~width:pt2_accumulator_bits ~f:(fun x ->
        let value = msbs ram_read0 in
        let valid = reg spec accum_valid.value in
        mux2 (valid &: Uop.(value >: x)) value x)
    in
    let () =
      let%tydi { read_data } =
        Ram.hierarchical
          ~rtl_attributes:
            ([ Option.map bel ~f:(fun x -> [%string "LOC=\"%{x}\""]) ] |> List.filter_opt)
          scope
          { clock
          ; clear
          ; load_ports = [| Ram.Port.unused; Ram.Port.unused |]
          ; load_finished = vdd
          ; ram_ports =
              [| { address = ram_address0.value
                 ; write_data = ram_write0_value.value
                 ; write_enable = ram_write0_en.value
                 }
               ; { address = pipeline ~n:2 spec accum_address.value
                 ; write_data = pipeline ~n:1 spec (msbs ram_read0 @: gnd)
                 ; write_enable = pipeline ~n:2 spec accum_valid.value
                 }
              |]
          }
      in
      ram_read0 <== reg spec read_data.(0)
    in
    compile
      [ ram_write0_en <-- gnd
      ; accum_valid <-- gnd
      ; sm.switch
          [ ( Idle
            , [ when_
                  (value_valid &: match_)
                  [ value_in_reg <-- value_to_add
                  ; sm.set_next Ram_read1
                  ; ram_address0 <-- address
                  ]
              ; when_
                  end_of_sequence
                  [ sm.set_next Accumulate_maximum
                  ; max_value_clear <-- vdd
                  ; ram_address0 <--. 0
                  ]
              ] )
          ; Ram_read1, [ sm.set_next Ram_read2 ]
          ; Ram_read2, [ sm.set_next Ram_write ]
          ; ( Ram_write
            , [ when_
                  ~:(lsb ram_read0)
                  [ ram_write0_en <-- vdd
                  ; ram_write0_value
                    <-- (msbs ram_read0
                         +: uresize ~width:pt2_accumulator_bits value_in_reg.value)
                        @: vdd
                  ]
              ; sm.set_next Idle
              ] )
          ; ( Accumulate_maximum
            , [ accum_valid <-- vdd
              ; accum_address <-- ram_address0.value
              ; incr ram_address0
              ; when_ (ram_address0.value ==:. ram_depth - 1) [ sm.set_next Idle ]
              ] )
          ]
      ];
    { max_value }
  ;;
end

module All_result_rams = struct
  let accumulate_cycles = Single_result_ram.accumulate_cycles + 20
  let ram_index_bits = Single_result_ram.ram_index_bits

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; address : 'a [@bits total_address_bits]
      ; value_to_add : 'a [@bits 4]
      ; value_valid : 'a
      ; end_of_sequence : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { max_value : 'a [@bits pt2_accumulator_bits] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ~ram_index
    scope
    ({ clock; clear; address; value_to_add; value_valid; end_of_sequence } : _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let address = pipeline ~n:2 spec address |> Signal.prev spec |> Staged.unstage in
    let value_to_add =
      pipeline ~n:2 spec value_to_add |> Signal.prev spec |> Staged.unstage
    in
    let value_valid =
      pipeline ~n:2 spec value_valid |> Signal.prev spec |> Staged.unstage
    in
    let end_of_sequence =
      pipeline ~n:2 spec end_of_sequence |> Signal.prev spec |> Staged.unstage
    in
    let results =
      List.init (1 lsl ram_index_bits) ~f:(fun ram_index ->
        let noc_index = ram_index lsr 2 in
        let%tydi { max_value } =
          Single_result_ram.create
            ~ram_index
            ~bel:"R33C5"
            scope
            { clock
            ; clear
            ; address = address noc_index |> reg spec
            ; value_to_add = value_to_add noc_index |> reg spec
            ; value_valid = value_valid noc_index |> reg spec
            ; end_of_sequence = end_of_sequence noc_index |> reg spec
            }
        in
        max_value)
    in
    let max_value = reduce results ~f:(fun a b -> reg spec (mux2 (a >: b) a b)) in
    { max_value }
  ;;
end

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let input = Util.shift_in ~clock ~clear ~n:4 uart_rx in
  let input_valid = input.valid in
  let input_address = sel_bottom ~width:(4 + total_address_bits) input.value in
  let input_value = sel_bottom ~width:4 input_address in
  let input_address = drop_bottom ~width:4 input_address in
  let part1_soln = gnd in
  let done_ = uart_rts in
  let%tydi { max_value } =
    All_result_rams.create
      ~ram_index:0
      scope
      { clock
      ; clear
      ; address = input_address
      ; value_to_add = input_value
      ; value_valid = input_valid &: (input.value <>:. 0)
      ; end_of_sequence = input.valid &: (input.value ==:. 0)
      }
  in
  let part2_soln = max_value in
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 part1_soln; valid = done_ }
      ; part2 = { value = uresize ~width:60 part2_soln; valid = done_ }
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

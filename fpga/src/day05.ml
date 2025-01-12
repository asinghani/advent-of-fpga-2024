open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 4096
let extra_synth_args = []
let page_num_bits = 7
let max_page_num = 1 lsl page_num_bits
let max_seq_len = 32

module Rules_ram = Loadable_pseudo_dual_port_ram.Make (struct
    let width = 1
    let depth = max_page_num * max_page_num
    let zero_on_startup = true
    let num_ports = 1
  end)

module Sequence_ram = Loadable_pseudo_dual_port_ram.Make (struct
    let width = page_num_bits
    let depth = max_seq_len
    let zero_on_startup = true
    let num_ports = 2
  end)

module States = struct
  type t =
    | Wait_for_ram_zero
    | Load_rules1
    | Load_rules2
    | Load_sequence
    | Sort_read1
    | Sort_read2
    | Sort_compare
    | Sort_write
    | Read_middle1
    | Read_middle2
    | Accumulate
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in
  ignore (sm.current -- "state");
  let%hw_var counter_outer =
    Variable.reg spec ~width:(num_bits_to_represent max_seq_len - 1)
  in
  let%hw_var counter_inner =
    Variable.reg spec ~width:(num_bits_to_represent max_seq_len - 1)
  in
  let%hw_var seq_length =
    Variable.reg spec ~width:(num_bits_to_represent max_seq_len - 1)
  in
  let%hw_var any_swapped = Variable.reg spec ~width:1 in
  let%hw_var rule_first = Variable.reg spec ~width:page_num_bits in
  let%hw_var rule_second = Variable.reg spec ~width:page_num_bits in
  let%hw_var rule_write = Variable.reg spec ~width:1 in
  let%hw a_gt_b = wire 1 in
  let%hw_var ram_zero_counter = Variable.reg spec ~width:(2 + (2 * page_num_bits)) in
  compile
    [ rule_write <-- gnd
    ; sm.switch
        [ ( Wait_for_ram_zero
          , [ incr ram_zero_counter
            ; when_
                (ram_zero_counter.value >=:. (max_page_num * max_page_num) + 128)
                [ sm.set_next Load_rules1 ]
            ] )
        ; ( Load_rules1
          , [ when_
                uart_rx.valid
                [ rule_first <-- uresize ~width:page_num_bits uart_rx.value
                ; sm.set_next Load_rules2
                ]
            ] )
        ; ( Load_rules2
          , [ when_
                uart_rx.valid
                [ rule_second <-- uresize ~width:page_num_bits uart_rx.value
                ; if_
                    (rule_first.value ==:. 0 &: (uart_rx.value ==:. 0))
                    [ sm.set_next Load_sequence ]
                  @@ else_ [ rule_write <-- vdd; sm.set_next Load_rules1 ]
                ]
            ] )
        ; ( Load_sequence
          , [ when_
                uart_rx.valid
                [ if_
                    (uart_rx.value ==:. 0)
                    [ counter_inner <--. 0
                    ; counter_outer <--. 0
                    ; any_swapped <-- gnd
                    ; sm.set_next Sort_read1
                    ]
                  @@ else_ [ incr seq_length ]
                ]
            ; when_ uart_rts [ sm.set_next Done ]
            ] )
          (* Reading takes two cycles, then reading the rule (to check which way
             the pair should be ordered) takes another cycle *)
        ; Sort_read1, [ sm.set_next Sort_read2 ]
        ; Sort_read2, [ sm.set_next Sort_compare ]
        ; Sort_compare, [ sm.set_next Sort_write ]
        ; ( Sort_write
          , [ sm.set_next Sort_read1
            ; when_ a_gt_b [ any_swapped <-- vdd ]
            ; incr counter_inner
            ; when_
                (counter_inner.value ==: seq_length.value -:. 2)
                [ counter_inner <--. 0
                ; incr counter_outer
                ; when_
                    (counter_outer.value ==: seq_length.value -:. 1)
                    [ counter_outer <--. 0 (* Read the middle element of the list *)
                    ; counter_inner <-- srl ~by:1 seq_length.value
                    ; sm.set_next Read_middle1
                    ]
                ]
            ] )
          (* The reading and accumulation happen below *)
        ; Read_middle1, [ sm.set_next Read_middle2 ]
        ; Read_middle2, [ sm.set_next Accumulate ]
        ; Accumulate, [ sm.set_next Load_sequence; seq_length <--. 0 ]
        ; Done, []
        ]
    ];
  let%hw write_larger = wire page_num_bits in
  let%hw write_smaller = wire page_num_bits in
  let read_a, read_b =
    let%hw write_enable = sm.is Sort_write in
    let%tydi { read_data } =
      Sequence_ram.hierarchical
        ~name:"seq_ram"
        scope
        { clock
        ; clear
        ; load_ports =
            [| { address = seq_length.value
               ; write_data = uresize ~width:page_num_bits uart_rx.value
               ; write_enable =
                   sm.is Load_sequence &: uart_rx.valid &: (uart_rx.value <>:. 0)
               }
             ; Sequence_ram.Port.unused
            |]
        ; load_finished = ~:(sm.is Load_sequence)
        ; ram_ports =
            [| { address = counter_inner.value; write_data = write_smaller; write_enable }
             ; { address = counter_inner.value +:. 1
               ; write_data = write_larger
               ; write_enable
               }
            |]
        }
    in
    reg spec read_data.(0), reg spec read_data.(1)
  in
  (a_gt_b
   <==
   let%tydi { read_data } =
     Rules_ram.hierarchical
       ~name:"rules_ram"
       scope
       { clock
       ; clear
       ; load_ports =
           [| { address = rule_second.value @: rule_first.value
              ; write_data = vdd
              ; write_enable = rule_write.value
              }
           |]
       ; load_finished = ~:(sm.is Load_rules1 |: sm.is Load_rules2)
       ; ram_ports =
           [| { address = read_a @: read_b; write_data = gnd; write_enable = gnd } |]
       }
   in
   read_data.(0));
  write_smaller <== mux2 a_gt_b read_b read_a;
  write_larger <== mux2 a_gt_b read_a read_b;
  let%hw uart_rx_ready = sm.is Load_rules1 |: sm.is Load_rules2 |: sm.is Load_sequence in
  let%hw part1_accum =
    reg_fb
      spec
      ~width:32
      ~enable:(sm.is Accumulate &: ~:(any_swapped.value))
      ~f:(fun x -> uresize ~width:(width x) Uop.(x +: read_a))
  in
  let%hw part2_accum =
    reg_fb
      spec
      ~width:32
      ~enable:(sm.is Accumulate &: any_swapped.value)
      ~f:(fun x -> uresize ~width:(width x) Uop.(x +: read_a))
  in
  let%hw done_ = sm.is Done in
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 part1_accum; valid = done_ }
      ; part2 = { value = uresize ~width:60 part2_accum; valid = done_ }
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

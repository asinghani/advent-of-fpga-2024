open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
    let width = 18
    let depth = 4096
    let zero_on_startup = false
    let num_ports = 2
  end)

module Loader = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a Uart.Byte_with_valid.t
      ; uart_rts : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { load_finished : 'a
      ; ram_write : 'a Ram.Port.t array [@length 2]
      ; data_length : 'a [@bits 12]
      ; uart_rx_ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope ({ clock; clear; uart_rx; uart_rts } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in
    let word_in = Util.shift_in ~clock ~clear ~n:4 uart_rx in
    let counter = reg_fb spec ~width:13 ~enable:word_in.valid ~f:(fun x -> x +:. 1) in
    let index = msbs counter in
    { load_finished = uart_rts
    ; ram_write =
        [| { address = index
           ; write_data = word_in.value |> sel_bottom ~width:18
           ; write_enable = word_in.valid &: (lsb counter ==:. 0)
           }
         ; { address = index
           ; write_data = word_in.value |> sel_bottom ~width:18
           ; write_enable = word_in.valid &: (lsb counter ==:. 1)
           }
        |]
    ; data_length = msbs counter
    ; uart_rx_ready = vdd
    }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~name:"loader" ~scope create
  ;;
end

module States = struct
  type t =
    | Loading
    | Sorting_read
    | Sorting_write
    | Summing
    | Read_part2
    | Searching_part2
    | Adding_part2
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let absdiff a b = mux2 (a >: b) (a -: b) (b -: a)

let algo
  ~clock
  ~clear
  ~(ram_ports0 : _ Ram.Port.t array)
  ~(ram_ports1 : _ Ram.Port.t array)
  ~(read_data0 : _ array)
  ~(read_data1 : _ array)
  ~load_finished
  ~data_length
  =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in
  let counter_outer = Variable.reg spec ~width:12 in
  let counter_inner0 = Variable.reg spec ~width:12 in
  let counter_inner1 = Variable.reg spec ~width:12 in
  let write_enable = Variable.wire ~default:gnd in
  let ram0_a_gt_b = read_data0.(0) >: read_data0.(1) in
  let ram0_larger = mux2 ram0_a_gt_b read_data0.(0) read_data0.(1) in
  let ram0_smaller = mux2 ram0_a_gt_b read_data0.(1) read_data0.(0) in
  let ram1_a_gt_b = read_data1.(0) >: read_data1.(1) in
  let ram1_larger = mux2 ram1_a_gt_b read_data1.(0) read_data1.(1) in
  let ram1_smaller = mux2 ram1_a_gt_b read_data1.(1) read_data1.(0) in
  let part1_accum_enable = Variable.reg spec ~width:1 in
  let done_ = Variable.reg spec ~width:1 in
  let part1_accum =
    reg_fb spec ~width:32 ~enable:part1_accum_enable.value ~f:(fun x ->
      uresize ~width:(width x) Uop.(x +: absdiff read_data0.(0) read_data1.(0)))
  in
  let part2_accum = Variable.reg spec ~width:32 in
  let match_count = Variable.reg spec ~width:12 in
  compile
    [ part1_accum_enable <-- gnd
    ; sm.switch
        [ ( Loading
          , [ when_
                load_finished
                [ sm.set_next Sorting_read
                ; counter_inner0 <--. 0
                ; counter_inner1 <--. 0
                ; counter_outer <--. 0
                ]
            ] )
          (* Bubble sort *)
        ; Sorting_read, [ sm.set_next Sorting_write ]
        ; ( Sorting_write
          , [ sm.set_next Sorting_read
            ; write_enable <-- vdd
            ; counter_inner0 <-- counter_inner0.value +:. 1
            ; counter_inner1 <-- counter_inner1.value +:. 1
            ; when_
                (counter_inner0.value ==: data_length -:. 2)
                [ counter_inner0 <--. 0
                ; counter_inner1 <--. 0
                ; counter_outer <-- counter_outer.value +:. 1
                ; when_
                    (counter_outer.value ==: data_length -:. 1)
                    [ counter_outer <--. 0; sm.set_next Summing ]
                ]
            ] )
        ; ( Summing
          , [ part1_accum_enable <-- vdd
            ; counter_inner0 <-- counter_inner0.value +:. 1
            ; counter_inner1 <-- counter_inner1.value +:. 1
            ; when_
                (counter_inner0.value ==: data_length -:. 1)
                [ counter_inner0 <--. 0; counter_inner1 <--. 0; sm.set_next Read_part2 ]
            ] )
        ; Read_part2, [ sm.set_next Searching_part2; incr counter_inner1 ]
        ; ( Searching_part2
          , [ when_ (read_data0.(0) ==: read_data1.(0)) [ incr match_count ]
            ; incr counter_inner1
            ; when_
                (counter_inner1.value ==: data_length -:. 1)
                [ sm.set_next Adding_part2 ]
            ] )
        ; ( Adding_part2
          , [ part2_accum
              <-- part2_accum.value
                  +: uresize
                       ~width:(width part2_accum.value)
                       (match_count.value *: read_data0.(0))
            ; match_count <--. 0
            ; incr counter_inner0
            ; counter_inner1 <--. 0
            ; sm.set_next Read_part2
            ; when_ (counter_inner0.value ==: data_length -:. 1) [ sm.set_next Done ]
            ] )
        ; Done, [ done_ <-- vdd ]
        ]
    ];
  ram_ports0.(0).address <== counter_inner0.value;
  ram_ports0.(1).address <== counter_inner0.value +:. 1;
  ram_ports1.(0).address <== counter_inner1.value;
  ram_ports1.(1).address <== counter_inner1.value +:. 1;
  ram_ports0.(0).write_data <== ram0_smaller;
  ram_ports0.(1).write_data <== ram0_larger;
  ram_ports1.(0).write_data <== ram1_smaller;
  ram_ports1.(1).write_data <== ram1_larger;
  ram_ports0.(0).write_enable <== write_enable.value;
  ram_ports0.(1).write_enable <== write_enable.value;
  ram_ports1.(0).write_enable <== write_enable.value;
  ram_ports1.(1).write_enable <== write_enable.value;
  part1_accum, part2_accum.value, done_.value
;;

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let%tydi { load_finished; ram_write; data_length; uart_rx_ready } =
    Loader.hierarchical scope { clock; clear; uart_rx; uart_rts }
  in
  let ram_ports0 = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in
  let ram_ports1 = Array.init 2 ~f:(fun _ -> Ram.Port.Of_signal.wires ()) in
  let%tydi { read_data = read_data0 } =
    Ram.hierarchical
      ~name:"ram0"
      scope
      { clock
      ; clear
      ; load_ports = [| ram_write.(0); Ram.Port.unused |]
      ; load_finished
      ; ram_ports = ram_ports0
      }
  in
  let%tydi { read_data = read_data1 } =
    Ram.hierarchical
      ~name:"ram1"
      scope
      { clock
      ; clear
      ; load_ports = [| ram_write.(1); Ram.Port.unused |]
      ; load_finished
      ; ram_ports = ram_ports1
      }
  in
  let part1_soln, part2_soln, done_ =
    algo
      ~clock
      ~clear
      ~ram_ports0
      ~ram_ports1
      ~read_data0
      ~read_data1
      ~load_finished
      ~data_length
  in
  let%tydi { byte_out } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = { value = uresize ~width:60 part1_soln; valid = done_ }
      ; part2 = { value = uresize ~width:60 part2_soln; valid = done_ }
      }
  in
  { leds = concat_lsb [ ~:clear; uart_rx_overflow; load_finished; zero 5 ]
  ; uart_tx = byte_out
  ; uart_rx_ready
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

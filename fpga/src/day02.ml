open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []
let reading_bits = 8
let max_num_readings = 8

let reports_are_valid_helper ~clock ~clear ~readings_list ~readings_list_valid =
  let spec = Reg_spec.create ~clock ~clear () in
  (* Every pair of adjacent elements *)
  let pairs_to_check =
    List.zip_exn (List.drop_last_exn readings_list) (List.tl_exn readings_list)
  in
  List.map pairs_to_check ~f:(fun (a, b) ->
    readings_list_valid
    &: (b -: a ==:. 1 |: (b -: a ==:. 2) |: (b -: a ==:. 3) |: (a ==:. 0) |: (b ==:. 0)))
  |> List.map ~f:(reg spec)
  |> tree ~arity:4 ~f:(reduce ~f:( &: ))
  |> reg spec
;;

let reports_are_valid ~clock ~clear ~readings_list ~readings_list_valid =
  reports_are_valid_helper ~clock ~clear ~readings_list ~readings_list_valid
  |: reports_are_valid_helper
       ~clock
       ~clear
       ~readings_list:(List.rev readings_list)
       ~readings_list_valid
;;

let remove_each_element list =
  List.init (List.length list) ~f:(fun i -> List.filteri list ~f:(fun j _ -> i <> j))
;;

let reports_are_valid_with_one_each_removed
  ~clock
  ~clear
  ~readings_list
  ~readings_list_valid
  =
  let spec = Reg_spec.create ~clock ~clear () in
  readings_list :: remove_each_element readings_list
  |> List.map ~f:(fun r ->
    reports_are_valid ~clock ~clear ~readings_list_valid ~readings_list:r)
  |> List.map ~f:(reg spec)
  |> tree ~arity:4 ~f:(reduce ~f:( |: ))
  |> reg spec
;;

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  (* Since readings are required to be nonzero, we can use a zero reading to
     fill unused space in the register, as well as to indicate the end of each
     set of samples. *)
  let shreg =
    reg_fb
      spec
      ~width:(max_num_readings * reading_bits)
      ~enable:uart_rx.valid
      ~f:(fun x ->
        mux2
          (uart_rx.value ==:. 0)
          (zero (width x))
          (drop_top ~width:reading_bits x @: uart_rx.value))
  in
  let readings_list = shreg |> split_msb ~exact:true ~part_width:reading_bits in
  let readings_list_valid = uart_rx.valid &: (uart_rx.value ==:. 0) in
  let reading_valid =
    reports_are_valid ~clock ~clear ~readings_list ~readings_list_valid
  in
  let reading_valid_with_one_removed =
    reports_are_valid_with_one_each_removed
      ~clock
      ~clear
      ~readings_list
      ~readings_list_valid
  in
  let%hw part1_accum =
    reg_fb spec ~width:16 ~enable:reading_valid ~f:(fun x -> x +:. 1)
  in
  let%hw part2_accum =
    reg_fb spec ~width:16 ~enable:reading_valid_with_one_removed ~f:(fun x -> x +:. 1)
  in
  let done_ = uart_rts in
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
  ; uart_rx_ready = vdd
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (Ulx3s.I) (Ulx3s.O) in
  Scoped.hierarchical_here ~here:[%here] ~scope create
;;

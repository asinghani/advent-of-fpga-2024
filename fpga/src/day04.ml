open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = [ "-dff" ]
let max_width_bits = 8
let max_width = 1 lsl max_width_bits

(* Collapse the chars XMAS down to a 2-bit value *)
let char_to_xmas char = String.index_exn "XMAS" char

let signal_to_xmas char =
  "XMAS"
  |> String.to_list
  |> List.fold ~init:(of_int ~width:2 0) ~f:(fun acc x ->
    mux2 (char ==:. Char.to_int x) (of_int ~width:2 (char_to_xmas x)) acc)
;;

let rec make_shreg ~n ~spec ~enable signal =
  match n with
  | 0 -> []
  | _ -> signal :: make_shreg ~n:(n - 1) ~spec ~enable (reg spec ~enable signal)
;;

(* Mux with a register between each level of the mux tree. Incredibly
   inefficient but this design is very routing constrained on the tiny ECP5
   since we support any arbitrary grid width which requires a very wide
   multiplexer to do in a streaming fashion. *)
let rec recursive_mux ~spec ~sel list =
  let _ = spec in
  assert (List.length list = 1 lsl width sel);
  match list with
  | [ a; b ] -> mux2 sel b a, 0
  | _ ->
    (match List.chunks_of ~length:(List.length list / 2) list with
     | [ first; second ] ->
       let a, depth_a = recursive_mux ~spec ~sel:(lsbs sel) first in
       let b, depth_b = recursive_mux ~spec ~sel:(lsbs sel) second in
       assert (depth_a = depth_b);
       reg spec (mux2 (msb sel) b a), 1 + depth_a
     | _ -> failwith "unreachable")
;;

let make_sliding_window
  scope
  ~clock
  ~clear
  ~width
  ~height
  ~(check_fn : Signal.t list list -> Signal.t)
  ~row_counter
  ~col_counter
  ~grid_width
  (value_in : _ With_valid.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let shift_reg =
    make_shreg ~n:(max_width * height) ~spec ~enable:value_in.valid value_in.value
  in
  let depth = ref 0 in
  let window =
    List.init height ~f:(fun i ->
      List.init width ~f:(fun j ->
        let signal, depth' =
          recursive_mux
            ~spec
            ~sel:(uresize ~width:max_width_bits grid_width)
            (List.init max_width ~f:(fun x -> List.nth_exn shift_reg ((x * i) + j)))
        in
        depth := depth';
        Scope.naming scope signal [%string "window_%{i#Int}_%{j#Int}x"]))
  in
  let%hw window_valid =
    value_in.valid &: (row_counter >=:. height - 1) &: (col_counter >=:. width - 1)
  in
  pipeline spec ~n:(!depth + 2) window_valid
  &: check_fn (window |> List.map ~f:(List.map ~f:(pipeline spec ~n:2)))
;;

let window_to_row window = window |> List.hd_exn
let window_to_col window = window |> List.transpose_exn |> List.hd_exn
let window_to_diag window = window |> List.mapi ~f:(fun i line -> List.nth_exn line i)

let window_to_rev_diag window =
  window |> List.rev |> List.mapi ~f:(fun i line -> List.nth_exn line i)
;;

let list_match_reversible ~clock ~clear ~pattern list =
  let match_forward = List.map2_exn list pattern ~f:Signal.( ==:. ) |> reduce ~f:( &: ) in
  let match_backward =
    List.map2_exn list (List.rev pattern) ~f:Signal.( ==:. ) |> reduce ~f:( &: )
  in
  match_forward |: match_backward
;;

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let is_end_of_line = uart_rx.valid &: (uart_rx.value ==:. Char.to_int '\n') in
  (* Measure the width of the grid by counting how long the first line is *)
  let width_known = reg_fb spec ~width:1 ~f:(fun x -> x |: is_end_of_line) in
  let grid_width =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:(uart_rx.valid &: ~:is_end_of_line &: ~:width_known)
      ~f:(fun i -> i +:. 1)
  in
  let col_counter =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:uart_rx.valid
      ~f:(fun i -> mux2 is_end_of_line (zero (width i)) (i +:. 1))
  in
  let row_counter =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:is_end_of_line
      ~f:(fun i -> i +:. 1)
  in
  let make_sliding_window =
    make_sliding_window
      scope
      ~clock
      ~clear
      ~row_counter
      ~col_counter
      ~grid_width
      { With_valid.valid = uart_rx.valid &: ~:is_end_of_line
      ; value = signal_to_xmas uart_rx.value
      }
  in
  let part1_count =
    [ (* Horizontal *)
      make_sliding_window ~width:4 ~height:1 ~check_fn:(fun window ->
        window
        |> window_to_row
        |> list_match_reversible
             ~clock
             ~clear
             ~pattern:(List.map [ 'X'; 'M'; 'A'; 'S' ] ~f:char_to_xmas))
      (* Vertical *)
    ; make_sliding_window ~width:1 ~height:4 ~check_fn:(fun window ->
        window
        |> window_to_col
        |> list_match_reversible
             ~clock
             ~clear
             ~pattern:(List.map [ 'X'; 'M'; 'A'; 'S' ] ~f:char_to_xmas))
      (* Forward diagonal *)
    ; make_sliding_window ~width:4 ~height:4 ~check_fn:(fun window ->
        window
        |> window_to_diag
        |> list_match_reversible
             ~clock
             ~clear
             ~pattern:(List.map [ 'X'; 'M'; 'A'; 'S' ] ~f:char_to_xmas))
      (* Reverse diagonal *)
    ; make_sliding_window ~width:4 ~height:4 ~check_fn:(fun window ->
        window
        |> window_to_rev_diag
        |> list_match_reversible
             ~clock
             ~clear
             ~pattern:(List.map [ 'X'; 'M'; 'A'; 'S' ] ~f:char_to_xmas))
    ]
    |> reduce ~f:Uop.( +: )
  in
  let part2_count =
    make_sliding_window ~width:3 ~height:3 ~check_fn:(fun window ->
      match window with
      | [ [ c0; _; c1 ]; [ _; center; _ ]; [ c2; _; c3 ] ] ->
        let center_match = center ==:. char_to_xmas 'A' in
        let pair1_match =
          [ c0; c3 ]
          |> list_match_reversible
               ~clock
               ~clear
               ~pattern:(List.map [ 'S'; 'M' ] ~f:char_to_xmas)
        in
        let pair2_match =
          [ c1; c2 ]
          |> list_match_reversible
               ~clock
               ~clear
               ~pattern:(List.map [ 'S'; 'M' ] ~f:char_to_xmas)
        in
        center_match &: pair1_match &: pair2_match
      | _ -> failwith "unreachable case")
  in
  let%hw part1_accum =
    reg_fb spec ~width:16 ~f:(fun x -> uresize ~width:(width x) Uop.(x +: part1_count))
  in
  let%hw part2_accum =
    reg_fb spec ~width:16 ~f:(fun x -> uresize ~width:(width x) Uop.(x +: part2_count))
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

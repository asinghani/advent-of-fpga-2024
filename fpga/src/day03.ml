open! Core
open! Hardcaml
open! Signal

let clock_freq = Ulx3s.Clock_freq.Clock_25mhz
let uart_fifo_depth = 32
let extra_synth_args = []

let matches_string list string =
  let list = List.slice list 0 (String.length string) in
  let chars = String.to_list string in
  List.map2_exn list chars ~f:(fun signal char -> signal ==:. Char.to_int char)
  |> reduce ~f:( &: )
;;

let is_numeral s = s >=:. Char.to_int '0' &: (s <=:. Char.to_int '9')
let of_numeral s = s -:. Char.to_int '0'

let create
  scope
  ({ clock; clear; buttons; uart_rx; uart_rts; uart_rx_overflow; uart_tx_ready } :
    _ Ulx3s.I.t)
  : _ Ulx3s.O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let lookback_len_chars = String.length "mul(AAA,BBB)" in
  (* Flush the shift register at the end to handle all of the values *)
  let flush_cycles = 16 in
  let%hw flush_ctr =
    reg_fb spec ~width:(num_bits_to_represent flush_cycles) ~enable:uart_rts ~f:(fun x ->
      mux2 (x >=:. flush_cycles) x (x +:. 1))
  in
  let%hw done_ = flush_ctr >=:. flush_cycles in
  let%hw byte_in_valid = uart_rx.valid |: (uart_rts &: ~:done_) in
  let%hw byte_in = mux2 uart_rx.valid uart_rx.value (zero 8) in
  let%hw shreg =
    reg_fb spec ~width:(8 * lookback_len_chars) ~enable:byte_in_valid ~f:(fun x ->
      drop_top ~width:8 x @: byte_in)
  in
  let shreg = shreg |> split_msb ~exact:true ~part_width:8 in
  let%hw is_do = matches_string shreg "do()" in
  let%hw is_don't = matches_string shreg "don't()" in
  let%hw is_maybe_mul = matches_string shreg "mul(" in
  let%hw do_accumulate =
    reg_fb spec ~width:1 ~clear_to:vdd ~f:(fun x ->
      mux2 is_do vdd @@ mux2 is_don't gnd @@ x)
  in
  let%tydi { valid; value } =
    List.init 3 ~f:(fun alen ->
      let alen = alen + 1 in
      List.init 3 ~f:(fun blen ->
        let blen = blen + 1 in
        (* For simplicity, pad with leading zeros *)
        let a_chars =
          List.init (3 - alen) ~f:(fun _ -> of_char '0') @ List.slice shreg 4 (4 + alen)
        in
        let b_chars =
          List.init (3 - blen) ~f:(fun _ -> of_char '0')
          @ List.slice shreg (4 + 1 + alen) (4 + 1 + alen + blen)
        in
        let comma = List.nth_exn shreg (4 + alen) in
        let paren = List.nth_exn shreg (4 + 1 + alen + blen) in
        let conditions =
          List.concat
            [ List.map a_chars ~f:is_numeral
            ; List.map b_chars ~f:is_numeral
            ; [ comma ==:. Char.to_int ','
              ; paren ==:. Char.to_int ')'
              ; is_maybe_mul
              ; byte_in_valid
              ]
            ]
        in
        let valid = reduce ~f:( &: ) conditions |> pipeline ~n:2 spec in
        let a =
          List.fold a_chars ~init:(of_int ~width:8 0) ~f:(fun acc x ->
            Uop.((acc *: of_int ~width:4 10) +: of_numeral x))
          |> reg spec
        in
        let b =
          List.fold b_chars ~init:(of_int ~width:8 0) ~f:(fun acc x ->
            Uop.((acc *: of_int ~width:4 10) +: of_numeral x))
          |> reg spec
        in
        let value = Uop.(a *: b) |> reg spec in
        { With_valid.valid; value }))
    |> List.concat
    |> priority_select
  in
  let%hw part1_accum =
    reg_fb spec ~width:48 ~enable:valid ~f:(fun x -> x +: uresize ~width:(width x) value)
  in
  let%hw part2_accum =
    reg_fb spec ~width:48 ~enable:(valid &: do_accumulate) ~f:(fun x ->
      x +: uresize ~width:(width x) value)
  in
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

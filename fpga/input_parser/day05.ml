open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  let lines = String.split_lines raw in
  let rules, sequences = List.split_while lines ~f:(fun x -> not (String.is_empty x)) in
  let sequences = List.tl_exn sequences in
  let rules = List.concat_map rules ~f:(fun s -> all_ints_unsigned s) in
  let sequences = List.concat_map sequences ~f:(fun s -> all_ints_unsigned s @ [ 0 ]) in
  if verbose then print_s [%message "rules" ~_:(rules : int list)];
  if verbose then print_s [%message "sequences" ~_:(sequences : int list)];
  List.map rules ~f:(fun x -> Uart_symbol.Byte (Char.of_int_exn x))
  (* (0, 0) = end of rules, start of sequences *)
  @ [ Uart_symbol.Byte '\000'; Uart_symbol.Byte '\000' ]
  @ List.map sequences ~f:(fun x -> Uart_symbol.Byte (Char.of_int_exn x))
  @ [ Uart_symbol.Rts true ]
;;

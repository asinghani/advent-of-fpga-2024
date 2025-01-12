open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let lines = String.split_lines raw in
  let values = List.concat_map lines ~f:(fun s -> all_ints_unsigned s @ [ 0 ]) in
  List.map values ~f:(fun x -> Uart_symbol.Byte (Char.of_int_exn x))
  @ [ Uart_symbol.Rts true ]
;;

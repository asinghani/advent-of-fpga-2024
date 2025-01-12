open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let values = all_ints_unsigned raw @ [ 0 ] in
  List.concat_map values ~f:(int_to_uart_bytes_le ~n:4)
;;

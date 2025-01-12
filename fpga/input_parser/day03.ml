open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  let values = raw |> String.strip |> String.to_list in
  List.map values ~f:(fun c -> Uart_symbol.Byte c) @ [ Uart_symbol.Rts true ]
;;

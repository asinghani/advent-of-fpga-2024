open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;
  (raw ^ "\n" |> String.to_list |> List.map ~f:(fun x -> Uart_symbol.Byte x))
  @ [ Uart_symbol.Rts true ]
;;

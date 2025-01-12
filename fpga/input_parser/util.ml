open! Core

module Uart_symbol = struct
  type t =
    | Byte of char
    | Rts of bool
  [@@deriving sexp]
end

module type Input_parser = sig
  val parse : ?verbose:bool -> string -> Uart_symbol.t list
end

let input_files =
  [%embed_file_as_string "input_parser/embedded.txt"]
  |> String.split_lines
  |> List.map ~f:(fun s ->
    match String.split ~on:' ' s with
    | [ filename; b64 ] ->
      let data = Base64.decode_exn b64 in
      filename, data
    | _ -> failwith "Invalid embedded.txt")
  |> Map.of_alist_exn (module String)
;;

let get_input_file filename = Map.find_exn input_files filename

let all_ints_unsigned s =
  let re = Re.Perl.compile_pat "\\d+" in
  Re.matches re s |> List.map ~f:Int.of_string
;;

let all_ints_signed s =
  let re = Re.Perl.compile_pat "-?\\d+" in
  Re.matches re s |> List.map ~f:Int.of_string
;;

let int_to_bytes_le ~n x =
  List.init n ~f:(fun i -> Char.of_int_exn ((x lsr (i * 8)) land 0xFF))
;;

let int_to_uart_bytes_le ~n x =
  int_to_bytes_le ~n x |> List.map ~f:(fun x -> Uart_symbol.Byte x)
;;

let%expect_test "test file loading" =
  print_endline (get_input_file "sample1a.txt");
  [%expect {|
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    |}];
  print_s [%message "" ~_:(get_input_file "sample1a.txt" |> all_ints_unsigned : int list)];
  [%expect {| (3 4 4 3 2 5 1 3 3 9 3 3) |}];
  print_s [%message "" ~_:("1 abc -2 -312 hello400" |> all_ints_signed : int list)];
  [%expect {| (1 -2 -312 400) |}]
;;

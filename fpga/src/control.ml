open! Core
module Uart_symbol = Advent_of_fpga_input_parser.Util.Uart_symbol

let designs =
  [ "day01", Advent_of_fpga_input_parser.Day01.parse
  ; "day02", Advent_of_fpga_input_parser.Day02.parse
  ; "day03", Advent_of_fpga_input_parser.Day03.parse
  ; "day04", Advent_of_fpga_input_parser.Day04.parse
  ; "day05", Advent_of_fpga_input_parser.Day05.parse
  ; "day07", Advent_of_fpga_input_parser.Day07.parse
  ; "day13", Advent_of_fpga_input_parser.Day13.parse
  ; "day22", Advent_of_fpga_input_parser.Day22.parse
  ]
;;

(* Embed the wrapper code so this binary can be standalone *)
let uart_wrapper = [%embed_file_as_string "uart_wrapper.py"]

let send_rts oc rts =
  let rts = Bool.to_int rts in
  Out_channel.output_string oc [%string "rts %{rts#Int}"]
;;

let send_dtr oc dtr =
  let dtr = Bool.to_int dtr in
  Out_channel.output_string oc [%string "dtr %{dtr#Int}"]
;;

let send_byte oc byte =
  let byte = Char.to_int byte in
  Out_channel.output_string oc [%string "b %{byte#Int.Hex}"]
;;

let send_inputs ~parse ~port ~filename =
  let data : Uart_symbol.t list = parse filename in
  let workdir = Filename_unix.temp_dir "fpga" "" in
  let uart_wrapper_file = workdir ^/ "uart_wrapper.py" in
  (* Write the temporary files *)
  Out_channel.with_file uart_wrapper_file ~f:(fun oc ->
    Out_channel.output_string oc uart_wrapper);
  let commands =
    data
    |> List.map ~f:(function
      | Uart_symbol.Byte b ->
        b |> Char.to_int |> Int.Hex.to_string |> String.chop_prefix_exn ~prefix:"0x"
      | Uart_symbol.Rts r -> if r then "rts1" else "rts0")
    |> String.concat ~sep:" "
  in
  let _ =
    Core_unix.system [%string "python3 %{uart_wrapper_file} %{port} 115200 2 %{commands}"]
  in
  ()
;;

let command =
  designs
  |> List.map ~f:(fun (name, parse) -> name, send_inputs ~parse)
  |> List.map
       ~f:
         (Tuple2.map_snd ~f:(fun fn ->
            Command.basic
              ~summary:"Interface with the FPGA"
              (let%map_open.Command filename = anon ("filename" %: string)
               and port = anon ("port" %: string) in
               fun () -> fn ~port ~filename)))
  |> Command.group ~summary:"Interface with the FPGA"
;;

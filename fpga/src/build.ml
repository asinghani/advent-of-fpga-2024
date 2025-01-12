open! Core
open! Hardcaml

let ( ^/ ) = Filename.concat

let designs =
  [ "blinky", (module Blinky : Ulx3s.Design)
  ; "uart-test", (module Uart_test : Ulx3s.Design)
  ; "day01", (module Day01 : Ulx3s.Design)
  ; "day02", (module Day02 : Ulx3s.Design)
  ; "day03", (module Day03 : Ulx3s.Design)
  ; "day04", (module Day04 : Ulx3s.Design)
  ; "day05", (module Day05 : Ulx3s.Design)
  ; "day07", (module Day07 : Ulx3s.Design)
  ; "day13", (module Day13 : Ulx3s.Design)
  ; "day22-pt1", (module Day22_pt1 : Ulx3s.Design)
  ]
;;

let constraints = [%embed_file_as_string "src/constraints.lpf"]

let command_exn ~workdir command =
  match Core_unix.system command with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%message
        "Error during command"
          (command : string)
          ~log_dir:(workdir : string)
          (error : Core_unix.Exit_or_signal.error)]
;;

let generate_rtl (module Design : Ulx3s.Design) () =
  let open Ulx3s_top.Make (Design) in
  let module Circuit = Circuit.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let circuit = Circuit.create_exn ~name:"hardcaml_ulx3s_top" (create scope) in
  let database = Scope.circuit_database scope in
  let buffer = Buffer.create 16 in
  Rtl.output
    ~database
    ~output_mode:(Rtl.Output_mode.To_buffer buffer)
    Rtl.Language.Verilog
    circuit;
  Buffer.contents buffer
;;

let synth_project (module Design : Ulx3s.Design) () =
  let rtl = generate_rtl (module Design : Ulx3s.Design) () in
  let workdir = Filename_unix.temp_dir "fpgabuild" "" in
  (* Write the temporary files *)
  Out_channel.with_file (workdir ^/ "top.sv") ~f:(fun oc ->
    Out_channel.output_string oc rtl);
  Out_channel.with_file (workdir ^/ "constraints.lpf") ~f:(fun oc ->
    Out_channel.output_string oc constraints);
  command_exn
    ~workdir
    [%string
      "yosys -p 'read_verilog -sv %{workdir}/top.sv; synth_ecp5 -json \
       %{workdir}/synthesis.json -top hardcaml_ulx3s_top -no-rw-check; tee -o \
       %{workdir}/utilization.log stat' | tee %{workdir}/synth.log"];
  ()
;;

let build_project ~bitstream_path (module Design : Ulx3s.Design) () =
  let rtl = generate_rtl (module Design : Ulx3s.Design) () in
  let workdir = Filename_unix.temp_dir "fpgabuild" "" in
  (* Write the temporary files *)
  Out_channel.with_file (workdir ^/ "top.sv") ~f:(fun oc ->
    Out_channel.output_string oc rtl);
  Out_channel.with_file (workdir ^/ "constraints.lpf") ~f:(fun oc ->
    Out_channel.output_string oc constraints);
  let extra_synth_args = Design.extra_synth_args |> String.concat ~sep:" " in
  command_exn
    ~workdir
    [%string
      "yosys -p 'read_verilog -sv %{workdir}/top.sv; synth_ecp5 -json \
       %{workdir}/synthesis.json -top hardcaml_ulx3s_top -no-rw-check \
       %{extra_synth_args}; tee -o %{workdir}/utilization.log stat' | tee \
       %{workdir}/synth.log"];
  command_exn ~workdir [%string "file -E %{workdir}/synthesis.json"];
  command_exn
    ~workdir
    [%string
      "nextpnr-ecp5 --85k --json %{workdir}/synthesis.json --lpf \
       %{workdir}/constraints.lpf --textcfg %{workdir}/pnr_out.config | tee \
       %{workdir}/pnr.log"];
  command_exn ~workdir [%string "file -E %{workdir}/pnr_out.config"];
  command_exn
    ~workdir
    [%string
      "ecppack --compress %{workdir}/pnr_out.config %{workdir}/bitstream.bit | tee \
       %{workdir}/ecppack.log"];
  command_exn ~workdir [%string "file -E %{workdir}/bitstream.bit"];
  command_exn ~workdir [%string "cp %{workdir}/bitstream.bit %{bitstream_path}"];
  command_exn ~workdir [%string "cat %{workdir}/utilization.log"];
  print_endline [%string "Bitstream saved to %{bitstream_path}"];
  print_endline [%string "Logs saved to %{workdir}"];
  ()
;;

let rtl_command =
  designs
  |> List.map ~f:(Tuple2.map_snd ~f:generate_rtl)
  |> List.map
       ~f:
         (Tuple2.map_snd ~f:(fun fn ->
            Command.basic
              ~summary:"Generate RTL"
              (let%map_open.Command () = return () in
               fun () -> print_endline (fn ()))))
  |> Command.group ~summary:"Generate RTL for each design"
;;

let synth_command =
  designs
  |> List.map ~f:(fun (name, (module Design : Ulx3s.Design)) ->
    name, synth_project (module Design))
  |> List.map
       ~f:
         (Tuple2.map_snd ~f:(fun fn ->
            Command.basic
              ~summary:"Synthesize the design"
              (let%map_open.Command () = return () in
               fun () -> fn ())))
  |> Command.group ~summary:"Synthesize each design"
;;

let build_command =
  designs
  |> List.map ~f:(fun (name, (module Design : Ulx3s.Design)) ->
    name, build_project ~bitstream_path:(name ^ ".bit") (module Design))
  |> List.map
       ~f:
         (Tuple2.map_snd ~f:(fun fn ->
            Command.basic
              ~summary:"Build the design"
              (let%map_open.Command () = return () in
               fun () -> fn ())))
  |> Command.group ~summary:"Build each design"
;;

let command =
  Command.group
    ~summary:"Advent of FPGA"
    [ "rtl", rtl_command; "synth", synth_command; "build", build_command ]
;;

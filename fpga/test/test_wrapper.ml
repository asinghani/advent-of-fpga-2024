open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Ulx3s = Advent_of_fpga.Ulx3s
module Sim = Cyclesim.With_interface (Ulx3s.I) (Ulx3s.O)

type t =
  { sim : Sim.t
  ; recv_buffer : char Queue.t
  ; cycles_between_bytes : int
  }

let create ~hierarchical ?waves ?(cycles_between_bytes = 30) () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let config =
    Cyclesim.Config.(
      add_random_initialization trace_all Random_initializer.(create randomize_all))
  in
  let sim = Sim.create ~config (hierarchical scope) in
  let waves, sim =
    match waves with
    | None -> None, sim
    | Some _ ->
      let waves, sim = Waveform.create sim in
      Some waves, sim
  in
  Core.at_exit (fun () ->
    Option.iter waves ~f:(fun waves ->
      Waveform.Serialize.marshall waves "/tmp/waves.hardcamlwaveform"));
  let i = Cyclesim.inputs sim in
  i.uart_rx.valid := Bits.gnd;
  i.uart_rts := Bits.gnd;
  i.uart_rx_overflow := Bits.gnd;
  i.uart_tx_ready := Bits.vdd;
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  { sim; recv_buffer = Queue.create (); cycles_between_bytes }
;;

let cycle ?(n = 1) { sim; recv_buffer; _ } =
  let o = Cyclesim.outputs sim in
  for _ = 1 to n do
    Cyclesim.cycle sim;
    if Bits.to_bool !(o.uart_tx.valid)
    then Queue.enqueue recv_buffer (Bits.to_char !(o.uart_tx.value))
  done
;;

let write_byte t byte =
  let i = Cyclesim.inputs t.sim in
  let o = Cyclesim.outputs t.sim in
  i.uart_rx.value := Bits.of_char byte;
  i.uart_rx.valid := Bits.vdd;
  let ctr = ref 0 in
  while not (Bits.to_bool !(o.uart_rx_ready)) do
    Int.incr ctr;
    cycle t
  done;
  cycle t;
  i.uart_rx.valid := Bits.gnd;
  cycle ~n:t.cycles_between_bytes t
;;

let set_rts t rts =
  let i = Cyclesim.inputs t.sim in
  i.uart_rts := Bits.of_bool rts;
  cycle ~n:1 t
;;

let feed_inputs t (inputs : Advent_of_fpga_input_parser.Util.Uart_symbol.t list) =
  List.iter inputs ~f:(function
    | Byte b -> write_byte t b
    | Rts r -> set_rts t r)
;;

let get_uart_output t = t.recv_buffer |> Queue.to_list |> String.of_char_list
let dump_uart_output t = get_uart_output t |> print_endline

let read_memory t name =
  Cyclesim.lookup_mem_by_name t.sim name |> Option.value_exn |> Cyclesim.Memory.read_all
;;

let read_memory_int t name = read_memory t name |> Array.map ~f:Bits.to_int

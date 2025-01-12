open! Core
open! Hardcaml
open! Hardcaml_waveterm

module Uart = Advent_of_fpga.Uart.Make (struct
    let baud = 10
    let clock_freq_hz = 100
    let rx_fifo_depth = 32
    let tx_fifo_depth = 32
  end)

let%expect_test "uart tx test" =
  let open Uart.Tx in
  let module Sim = Cyclesim.With_interface (I) (O) in
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (create scope) in
  let i = Cyclesim.inputs sim in
  let waves, sim = Waveform.create sim in
  let cycle ?(n = 1) () =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  i.clear := Bits.vdd;
  cycle ();
  i.clear := Bits.gnd;
  cycle ();
  i.byte_in.valid := Bits.gnd;
  cycle ();
  i.byte_in.valid := Bits.vdd;
  i.byte_in.value := Bits.of_int_trunc ~width:8 0x88;
  cycle ();
  i.byte_in.valid := Bits.gnd;
  cycle ~n:150 ();
  let display_rules =
    [ Display_rule.Custom
        (fun port ->
          if String.is_prefix (Port_name.to_string port.port_name) ~prefix:"fifo$"
          then None
          else Some Wave_format.Binary)
    ]
  in
  Waveform.expect
    waves
    ~display_rules
    ~wave_width:(-2)
    ~wave_height:1
    ~display_height:32
    ~display_width:80;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││─╥────────────────────────────────────────────────────────│
    │byte_in$valid     ││ ║ 0                                                      │
    │                  ││─╨────────────────────────────────────────────────────────│
    │                  ││─╥────────────────────────────────────────────────────────│
    │byte_in$value     ││ ║ 10001000                                               │
    │                  ││─╨────────────────────────────────────────────────────────│
    │                  ││╥─────────────────────────────────────────────────────────│
    │clear             ││║ 0                                                       │
    │                  ││╨─────────────────────────────────────────────────────────│
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │                  ││──────────────────────────────────────────────────────────│
    │ready             ││ 1                                                        │
    │                  ││──────────────────────────────────────────────────────────│
    │                  ││──╥───────────────────╥────╥──────────────╥───────────────│
    │tx                ││ 1║ 0                 ║ 1  ║ 0            ║ 1             │
    │                  ││──╨───────────────────╨────╨──────────────╨───────────────│
    │                  ││──╥─────────╥────╥────╥────╥────╥────╥────╥────╥──────────│
    │byte_to_send      ││ .║ 10001000║ 01.║ 00.║ 00.║ 00.║ 00.║ 00.║ 00.║ 00000000 │
    │                  ││──╨─────────╨────╨────╨────╨────╨────╨────╨────╨──────────│
    │                  ││──╥───────────────────────────────────────────────────────│
    │fifo_empty        ││ 1║ 1                                                     │
    │                  ││──╨───────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────│
    │fifo_full         ││ 0                                                        │
    │                  ││──────────────────────────────────────────────────────────│
    │                  ││──╥───────────────────────────────────────────────────────│
    │fifo_out          ││ .║ 00000000                                              │
    │                  ││──╨───────────────────────────────────────────────────────│
    │                  ││──╥───────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    b10c8b77023ed1bbf53ed671b4e22aed
    |}]
;;

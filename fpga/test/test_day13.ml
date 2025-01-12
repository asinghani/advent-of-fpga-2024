open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day13
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~cycles_between_bytes:200 ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day13.parse ~verbose:true "sample13a.txt" in
  [%expect
    {|
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    |}];
  feed_inputs sim inputs;
  cycle ~n:100 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 480
    Part 2: 875318608908
    |}]
;;

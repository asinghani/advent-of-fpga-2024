open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day02
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day02.parse ~verbose:true "sample2a.txt" in
  [%expect
    {|
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    |}];
  feed_inputs sim inputs;
  cycle ~n:100 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 2
    Part 2: 4
    |}]
;;

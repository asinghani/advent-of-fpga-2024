open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day22_pt1
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~cycles_between_bytes:1000 ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day22.parse ~verbose:true "sample22a.txt" in
  [%expect
    {|
    1
    10
    100
    2024
    |}];
  feed_inputs sim inputs;
  cycle ~n:100 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 37327623
    Part 2:
    |}]
;;

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day07
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day07.parse ~verbose:true "sample7a.txt" in
  [%expect
    {|
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    |}];
  cycle ~n:100 sim;
  feed_inputs sim inputs;
  cycle ~n:10000 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 3749
    Part 2: 11387
    |}]
;;

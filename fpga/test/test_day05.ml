open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day05
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day05.parse ~verbose:true "sample5a.txt" in
  [%expect
    {|
    (rules
     (47 53 97 13 97 61 97 47 75 29 61 13 75 53 29 13 97 29 53 29 61 53 97 53 61
      29 47 13 75 47 97 75 47 61 75 61 47 29 75 13 53 13))
    (sequences
     (75 47 61 53 29 0 97 61 53 29 13 0 75 29 13 0 75 97 47 61 53 0 61 13 29 0 97
      13 75 29 47 0))
    |}];
  cycle ~n:20000 sim;
  feed_inputs sim inputs;
  cycle ~n:1000 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 143
    Part 2: 123
    |}]
;;

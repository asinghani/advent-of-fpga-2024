open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day03
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day03.parse ~verbose:true "sample3b.txt" in
  [%expect {| xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5)) |}];
  feed_inputs sim inputs;
  cycle ~n:100 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 161
    Part 2: 48
    |}]
;;

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day04
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day04.parse ~verbose:true "sample4a.txt" in
  [%expect
    {|
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    |}];
  feed_inputs sim inputs;
  cycle ~n:100 sim;
  dump_uart_output sim;
  [%expect {|
    Part 1: 18
    Part 2: 9
    |}]
;;

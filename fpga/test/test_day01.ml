open! Core
open! Hardcaml
open! Hardcaml_waveterm
open Advent_of_fpga.Day01
module Ulx3s = Advent_of_fpga.Ulx3s
open Test_wrapper

let%expect_test "load data test" =
  let sim = create ~hierarchical () in
  let inputs = Advent_of_fpga_input_parser.Day01.parse ~verbose:true "sample1a.txt" in
  [%expect {|
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    |}];
  feed_inputs sim inputs;
  print_s [%message "" ~_:(Array.slice (read_memory_int sim "ram0") 0 6 : int array)];
  print_s [%message "" ~_:(Array.slice (read_memory_int sim "ram1") 0 6 : int array)];
  [%expect {|
    (3 4 2 1 3 3)
    (4 3 5 3 9 3)
      |}];
  cycle ~n:1000 sim;
  (* Validate sorted *)
  print_s [%message "" ~_:(Array.slice (read_memory_int sim "ram0") 0 6 : int array)];
  print_s [%message "" ~_:(Array.slice (read_memory_int sim "ram1") 0 6 : int array)];
  [%expect {|
    (1 2 3 3 3 4)
    (3 3 3 4 5 9)
    |}];
  dump_uart_output sim;
  [%expect {|
    Part 1: 11
    Part 2: 31
    |}]
;;

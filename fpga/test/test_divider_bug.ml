open! Core
open! Hardcaml
open! Hardcaml_waveterm

let width = 3

module Divider = Hardcaml_circuits.Divider.Make (struct
    let width = width
    let signedness = Signedness.Unsigned
    let architecture = Hardcaml_circuits.Divider.Architecture.Iterative
  end)

open Divider
module Sim = Cyclesim.With_interface (I) (O)

let numerator = 6
let denominator = 2

let%expect_test "iterative divider bug" =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let config =
    Cyclesim.Config.(
      add_random_initialization trace_all Random_initializer.(create randomize_all))
  in
  let sim = Sim.create ~config (hierarchical scope) in
  let waves, sim = Waveform.create sim in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  i.numerator := Bits.of_unsigned_int ~width numerator;
  i.denominator := Bits.of_unsigned_int ~width denominator;
  i.start := Bits.vdd;
  Cyclesim.cycle sim;
  i.start := Bits.gnd;
  Cyclesim.cycle sim;
  while not (Bits.to_bool !(o.valid)) do
    Cyclesim.cycle sim
  done;
  let () =
    let quotient = Bits.to_int !(o.quotient) in
    let remainder = Bits.to_int !(o.remainder) in
    print_s [%message "hardware" (quotient : int) (remainder : int)]
  in
  let () =
    let quotient = numerator / denominator in
    let remainder = numerator mod denominator in
    assert ((quotient * denominator) + remainder = numerator);
    print_s [%message "software" (quotient : int) (remainder : int)]
  in
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  [%expect
    {|
    (hardware (quotient 2) (remainder 0))
    (software (quotient 3) (remainder 0))
    |}]
;;

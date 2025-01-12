open! Core
open! Hardcaml

(* Take a with-valid signal of a given width, and shift it [n] times into a
   shift register before returning the output as a with-valid signal *)
val shift_in
  :  clock:Signal.t
  -> clear:Signal.t
  -> n:int
  -> ?ready:Signal.t
  -> Signal.t With_valid.t
  -> Signal.t With_valid.t

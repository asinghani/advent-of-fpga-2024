open! Core
open! Hardcaml
open! Signal

let shift_in ~clock ~clear ~n ?(ready = vdd) (signal : _ With_valid.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let valid_in = ready &: signal.valid in
  let counter =
    reg_fb spec ~width:(num_bits_to_represent n) ~enable:valid_in ~f:(fun x ->
      mux2 (x ==:. n - 1) (zero (width x)) (x +:. 1))
  in
  let shreg =
    reg_fb
      spec
      ~width:(n * width signal.value)
      ~enable:valid_in
      ~f:(fun x -> drop_bottom ~width:(width signal.value) (signal.value @: x))
  in
  let valid = reg spec (valid_in &: (counter ==:. n - 1)) in
  { With_valid.valid; value = shreg }
;;

(** Top level for the ULX3S board, sets up the clock and UART and maps pins to
    match the names in the board constraints *)

open! Core
open! Hardcaml

module Make (Design : Ulx3s.Design) : sig
  (** ULX3S inputs, names matched to [constraints.lpf] *)
  module I : sig
    type 'a t =
      { clock25 : 'a
      ; btn : 'a
      ; ftdi_txd : 'a
      ; ftdi_nrts : 'a
      ; ftdi_ndtr : 'a
      }
    [@@deriving hardcaml]
  end

  (** ULX3S outputs, names matched to [constraints.lpf] *)
  module O : sig
    type 'a t =
      { led : 'a
      ; ftdi_rxd : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

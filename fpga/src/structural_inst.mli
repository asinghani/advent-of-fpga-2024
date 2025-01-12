(** An experimental way of instantiating hardcaml modules in a chisel-like
    style. *)

open! Core
open! Hardcaml

module Make (Module : sig
    module I : Interface.S
    module O : Interface.S

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  end) : sig
  module IO : sig
    type 'a t =
      { i : 'a Module.I.t
      ; o : 'a Module.O.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t IO.t
end

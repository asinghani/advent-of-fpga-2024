open! Core
open! Hardcaml

module Make (Module : sig
    module I : Interface.S
    module O : Interface.S

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  end) =
struct
  module IO = struct
    type 'a t =
      { i : 'a Module.I.t
      ; o : 'a Module.O.t
      }
    [@@deriving hardcaml]
  end

  let create scope =
    let i = Module.I.Of_signal.wires () in
    let o = Module.create scope i in
    { IO.i; o }
  ;;
end

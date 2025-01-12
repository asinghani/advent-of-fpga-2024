open! Core
open! Hardcaml
module Solution : Interface.S with type 'a t = 'a With_valid.t

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; part1 : 'a Solution.t
    ; part2 : 'a Solution.t
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { byte_out : 'a Uart.Byte_with_valid.t } [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t

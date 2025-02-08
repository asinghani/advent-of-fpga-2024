open! Core
open! Hardcaml

(** Block RAM with an extra multiplexed write port, which can be used to load
    initial inputs into the RAM and then disabled once the data is loaded. *)
module Make (Config : sig
    val width : int
    val depth : int
    val zero_on_startup : bool
    val num_ports : int
  end) : sig
  val address_bits : int
  val latency : int

  module Port : sig
    type 'a t =
      { address : 'a
      ; write_data : 'a
      ; write_enable : 'a
      }
    [@@deriving hardcaml]

    val unused : Signal.t t
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; load_ports : 'a Port.t array
      ; load_finished : 'a
      ; ram_ports : 'a Port.t array
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { read_data : 'a array } [@@deriving hardcaml]
  end

  val hierarchical
    :  ?name:string
    -> ?rtl_attributes:string list
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end

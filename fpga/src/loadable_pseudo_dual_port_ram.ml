open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val width : int
    val depth : int
    val zero_on_startup : bool
    val num_ports : int
  end) =
struct
  open Config

  let address_bits = num_bits_to_represent (depth - 1)
  let latency = 1

  module Port = struct
    type 'a t =
      { address : 'a [@bits address_bits]
      ; write_data : 'a [@bits width]
      ; write_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]

    let unused = Of_signal.of_int 0
  end

  module Read_port = struct
    type 'a t = { address : 'a [@bits address_bits] } [@@deriving hardcaml ~rtlmangle:"$"]

    let unused = Of_signal.of_int 0
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; load_ports : 'a Port.t array [@length num_ports]
      ; load_finished : 'a
      ; ram_ports : 'a Port.t array [@length num_ports]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { read_data : 'a array [@length num_ports] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ?name
    ?(rtl_attributes = [])
    scope
    ({ clock; clear; load_ports; load_finished; ram_ports } : _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let init_in_progress = wire 1 in
    let init_counter =
      reg_fb spec ~width:(address_bits + 1) ~f:(fun x ->
        mux2 init_in_progress (x +:. 1) x)
    in
    init_in_progress <== if zero_on_startup then init_counter <:. depth else gnd;
    let init_ram_ports =
      Array.init num_ports ~f:(fun _ ->
        { Port.address = lsbs init_counter; write_data = zero width; write_enable = vdd })
    in
    let ram_ports =
      Array.init num_ports ~f:(fun i ->
        let init_port = init_ram_ports.(i) in
        let load_port = load_ports.(i) in
        let ram_port = ram_ports.(i) in
        Port.Of_signal.(
          mux2 init_in_progress init_port @@ mux2 ~:load_finished load_port ram_port))
    in
    let read_data =
      Signal.multiport_memory
        ?name
        ~attributes:(List.map rtl_attributes ~f:(fun x -> Rtl_attribute.create x))
        depth
        ~write_ports:
          (Array.map ram_ports ~f:(fun { address; write_data; write_enable } ->
             { Hardcaml.Write_port.write_clock = clock
             ; write_address = address
             ; write_enable
             ; write_data
             }))
        ~read_addresses:(Array.map ram_ports ~f:(fun { address; _ } -> address))
      |> Array.map ~f:(reg (Reg_spec.create ~clock ()))
    in
    { read_data }
  ;;

  let hierarchical ?name ?rtl_attributes scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical_here ~here:[%here] ~scope (create ?name ?rtl_attributes)
  ;;
end

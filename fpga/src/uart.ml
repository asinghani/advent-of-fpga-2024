open! Core
open! Hardcaml
open! Signal

module Byte_with_valid = With_valid.Vector (struct
    let width = 8
  end)

module Make (Config : sig
    val baud : int
    val clock_freq_hz : int
    val rx_fifo_depth : int
    val tx_fifo_depth : int
  end) =
struct
  open Config

  let clocks_per_baud = clock_freq_hz / baud
  let clocks_per_half_baud = clocks_per_baud / 2

  module States = struct
    type t =
      | Idle
      | Start
      | Data0
      | Data1
      | Data2
      | Data3
      | Data4
      | Data5
      | Data6
      | Data7
      | Stop
    [@@deriving sexp_of, compare, enumerate, string, variants]

    let next state = List.nth_exn all (Variants.to_rank state + 1)
  end

  module Rx = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; ready : 'a
        ; rx : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module O = struct
      type 'a t =
        { byte_out : 'a Byte_with_valid.t
        ; fifo_overflow : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    let create scope ({ clock; clear; ready; rx } : _ I.t) : _ O.t =
      let open Always in
      let spec = Reg_spec.create ~clock ~clear () in
      let sm = State_machine.create (module States) spec in
      let after_n_clocks ~n logic =
        let counter = Variable.reg ~width:(num_bits_to_represent n) spec in
        proc
          [ counter <-- counter.value +:. 1
          ; when_ (counter.value ==:. n - 1) [ counter <--. 0; proc logic ]
          ]
      in
      let%hw_var byte_rx = Variable.reg ~width:8 spec in
      let%hw_var fifo_wr = Variable.wire ~default:gnd in
      compile
        [ sm.switch
            (List.concat
               ([ [ Idle, [ when_ ~:rx [ sm.set_next Start ] ]
                  ; ( Start
                    , [ after_n_clocks
                          ~n:clocks_per_half_baud
                          [ if_ ~:rx [ sm.set_next Data0 ] @@ else_ [ sm.set_next Idle ] ]
                      ] )
                  ]
                ; List.map
                    ([ Data0; Data1; Data2; Data3; Data4; Data5; Data6; Data7 ]
                     : States.t list)
                    ~f:(fun state ->
                      ( state
                      , [ after_n_clocks
                            ~n:clocks_per_baud
                            [ byte_rx <-- rx @: msbs byte_rx.value
                            ; sm.set_next (States.next state)
                            ]
                        ] ))
                ; [ ( Stop
                    , [ after_n_clocks
                          ~n:clocks_per_baud
                          [ (* Validate the stop bit *)
                            when_ rx [ fifo_wr <-- vdd ]
                          ; sm.set_next Idle
                          ]
                      ] )
                  ]
                ]
                : (States.t * _) list list))
        ];
      let%hw fifo_rd = wire 1 in
      let%tydi { q = fifo_out; full = fifo_full; empty = fifo_empty; _ } =
        Fifo.create
          ~showahead:true
          ~scope:(Scope.sub_scope scope "fifo")
          ~capacity:rx_fifo_depth
          ~overflow_check:true
          ~underflow_check:true
          ~clock
          ~clear
          ~wr:fifo_wr.value
          ~d:byte_rx.value
          ~rd:fifo_rd
          ()
      in
      fifo_rd <== (~:fifo_empty &: ready);
      { byte_out = { value = fifo_out; valid = ~:fifo_empty }
      ; fifo_overflow =
          reg_fb spec ~width:1 ~f:(fun x -> x |: fifo_full) (* Overflow is sticky *)
      }
    ;;
  end

  module Tx = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; byte_in : 'a Byte_with_valid.t
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module O = struct
      type 'a t =
        { ready : 'a
        ; tx : 'a
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module States = struct
      type t =
        | Idle
        | Start
        | Data0
        | Data1
        | Data2
        | Data3
        | Data4
        | Data5
        | Data6
        | Data7
        | Stop
      [@@deriving sexp_of, compare, enumerate, string, variants]

      let next state = List.nth_exn all (Variants.to_rank state + 1)
    end

    let create scope ({ clock; clear; byte_in } : _ I.t) : _ O.t =
      let open Always in
      let spec = Reg_spec.create ~clock ~clear () in
      let%hw_var fifo_rd = Variable.wire ~default:gnd in
      let%tydi { q = fifo_out; full = fifo_full; empty = fifo_empty; _ } =
        Fifo.create
          ~showahead:true
          ~scope:(Scope.sub_scope scope "fifo")
          ~capacity:tx_fifo_depth
          ~overflow_check:true
          ~underflow_check:true
          ~clock
          ~clear
          ~wr:byte_in.valid
          ~d:byte_in.value
          ~rd:fifo_rd.value
          ()
      in
      let%hw fifo_full = fifo_full in
      let%hw fifo_empty = fifo_empty in
      let%hw fifo_out = fifo_out in
      let sm = State_machine.create (module States) spec in
      let after_n_clocks ~n logic =
        let counter = Variable.reg ~width:(num_bits_to_represent n) spec in
        proc
          [ counter <-- counter.value +:. 1
          ; when_ (counter.value ==:. n - 1) [ counter <--. 0; proc logic ]
          ]
      in
      let%hw_var byte_to_send = Variable.reg ~width:8 spec in
      let%hw_var tx = Variable.wire ~default:vdd in
      compile
        [ sm.switch
            (List.concat
               ([ [ ( Idle
                    , [ tx <-- vdd
                      ; when_
                          ~:fifo_empty
                          [ fifo_rd <-- vdd
                          ; byte_to_send <-- fifo_out
                          ; sm.set_next Start
                          ]
                      ] )
                  ; ( Start
                    , [ tx <-- gnd
                      ; after_n_clocks ~n:clocks_per_baud [ sm.set_next Data0 ]
                      ] )
                  ]
                ; List.map
                    ([ Data0; Data1; Data2; Data3; Data4; Data5; Data6; Data7 ]
                     : States.t list)
                    ~f:(fun state ->
                      ( state
                      , [ tx <-- lsb byte_to_send.value
                        ; after_n_clocks
                            ~n:clocks_per_baud
                            [ byte_to_send <-- gnd @: msbs byte_to_send.value
                            ; sm.set_next (States.next state)
                            ]
                        ] ))
                ; [ ( Stop
                    , [ tx <-- vdd
                      ; after_n_clocks ~n:(2 * clocks_per_baud) [ sm.set_next Idle ]
                      ] )
                  ]
                ]
                : (States.t * _) list list))
        ];
      { ready = ~:fifo_full; tx = tx.value }
    ;;
  end
end

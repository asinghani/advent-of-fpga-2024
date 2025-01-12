open! Core

module Uart_symbol : sig
  type t =
    | Byte of char
    | Rts of bool
  [@@deriving sexp]
end

module type Input_parser = sig
  val parse : ?verbose:bool -> string -> Uart_symbol.t list
end

val get_input_file : string -> string
val all_ints_unsigned : string -> int list
val all_ints_signed : string -> int list
val int_to_bytes_le : n:int -> int -> char list
val int_to_uart_bytes_le : n:int -> int -> Uart_symbol.t list

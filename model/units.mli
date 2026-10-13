(** Amounts as whole numbers of a currency's smallest unit (cents, satoshi, wei), and
    their decimal text. Zarith keeps 18-decimal ETH exact; js_of_ocaml's [int] is only 32
    bits, so even euro cents would overflow it past 21 million euros. *)

open! Core
open Neobank_api_types

type t = Z.t [@@deriving sexp_of, compare, equal]

(** Digits with an optional ['.'] and at most the currency's exponent decimals.
    Surrounding spaces and [','] or ['_'] group separators in the integer part are
    accepted; signs, exponents and empty input are not. *)
val parse : currency:string -> string -> t Or_error.t

(** [parse] that also refuses zero. *)
val parse_positive : currency:string -> string -> t Or_error.t

(** Plain decimal text the API accepts: fiat with exactly its decimals (["12.30"]),
    crypto without trailing zeros (["0.015"], ["2"]). Negative amounts get a ['-']. *)
val to_string : currency:string -> t -> string

val of_amount : Api_types.Amount.t -> t Or_error.t
val to_amount : currency:string -> t -> Api_types.Amount.t

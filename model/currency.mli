(** Currencies the app knows how to show and parse. Amounts travel as decimal strings
    (see [Api_types.Amount]); the exponent says how many decimals a currency allows. *)

open! Core

type kind =
  | Fiat
  | Crypto
[@@deriving sexp, compare, equal]

type t =
  { code : string
  ; name : string
  ; exponent : int (** decimals of the smallest unit: EUR 2, BTC 8, ETH 18 *)
  ; kind : kind
  ; symbol : string option (** written before the number, e.g. ["€"] *)
  ; networks : string list (** on-chain networks deposits and withdrawals may use *)
  }
[@@deriving sexp, compare, equal]

(** EUR, BTC, ETH, USDC, USDT in that order. *)
val all : t list

(** Case-insensitive. *)
val find : string -> t option

(** [find], or a fiat-like entry with 2 decimals for a code the app does not list, so an
    unexpected currency from the API still renders. *)
val get : string -> t

val is_crypto : string -> bool

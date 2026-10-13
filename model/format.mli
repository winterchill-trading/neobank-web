(** Text for people: amounts with group separators and symbols, times, IBANs in groups. *)

open! Core
open Neobank_api_types

(** ["€1,234.56"], ["-€12.50"], ["0.015 BTC"]. Text the app cannot parse is shown as it
    came, followed by the currency. *)
val amount : Api_types.Amount.t -> string

(** [amount] with a leading ['+'] for positive values, for movements. *)
val signed_amount : Api_types.Amount.t -> string

(** The API's ISO 8601 UTC times as ["2026-10-14 08:30 UTC"]; anything else unchanged. *)
val time : string -> string

(** ["2026-10-14"] from an ISO time or date. *)
val date : string -> string

(** Groups of four: ["DE89 3704 0044 0532 0130 00"]. *)
val iban : string -> string

(** ["bc1qxy...0wlh"]: the first [keep] and last 4 characters of a long address. *)
val short : ?keep:int -> string -> string

(** ["pending_review"] -> ["Pending review"]. *)
val status : string -> string

(** A statement as CSV, built in the browser from the statement the page already shows.
    Same columns as the API's own CSV: [at,amount,currency,code,reference,balance]. Fields
    are quoted per RFC 4180 when they hold a comma, quote or line break, and a field
    starting with [=], [+], [-] or [@] that is not a number gets a leading ['] so a
    spreadsheet does not run it as a formula. *)

open! Core
open Neobank_api_types

val to_string : Api_types.Statement.t -> string

(** ["statement-<account>-<from>-<until>.csv"] with dates only. *)
val file_name : Api_types.Statement.t -> string

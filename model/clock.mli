(** Times as the API writes them. The browser clock is a float of milliseconds since the
    epoch: js_of_ocaml's [int] is 32 bits and cannot hold it. *)

open! Core

(** ["2026-10-14T08:30:12.123Z"]. *)
val iso : float -> string

(** ["2026-10-14"]. *)
val date : float -> string

val minute_ms : float
val day_ms : float

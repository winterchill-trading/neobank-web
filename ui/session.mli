(** The signed-in customer, as far as the app needs to know. *)

open! Core

type t =
  { token : string
  ; customer_id : string
  }
[@@deriving sexp, equal]

(** Kept in [sessionStorage] for the live API so a reload keeps the session (the API still
    ends it after 5 idle minutes); never for the mock, whose bank starts afresh on every
    load. *)
val save : t option -> unit

val restore : unit -> t option

(** Navigation through the URL fragment: setting it fires [hashchange], which the app
    turns into its current route. *)

open! Core
open! Bonsai_web

val current : unit -> Neobank_web_model.Route.t
val navigate : Neobank_web_model.Route.t -> unit Effect.t

(** Calls [f] on every [hashchange] with the new route. *)
val on_change : (Neobank_web_model.Route.t -> unit) -> unit

(** The page's query string as key/value pairs, URL-decoded. *)
val query : unit -> (string * string) list

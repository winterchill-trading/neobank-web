(** Home: balances per currency and the latest payments. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** The latest payments as a table (also used by the account page). *)
val activity : Neobank_api_types.Api_types.Transfer.t list -> Vdom.Node.t

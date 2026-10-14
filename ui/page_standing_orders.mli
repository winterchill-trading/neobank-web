(** Standing orders: a fixed amount to a payee every day, week or month. The provider
    confirms a new order with its own code. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** ["monthly:15"] -> ["Monthly on day 15"], ... *)
val schedule_text : string -> string

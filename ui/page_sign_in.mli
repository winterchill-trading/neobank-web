(** Email, password and an authenticator code: a login is strong customer
    authentication. *)

open! Core
open! Bonsai_web

val component
  :  Page.t
  -> login:(Session.t -> unit Effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

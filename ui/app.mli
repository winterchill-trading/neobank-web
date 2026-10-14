(** The whole app: header with navigation and session, and the page for the current
    route. Every page component stays instantiated so its form input survives
    navigation; a page loads its data only while it is showing. *)

open! Core
open! Bonsai_web

val component
  :  backend:Backend.t
  -> route:Neobank_web_model.Route.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

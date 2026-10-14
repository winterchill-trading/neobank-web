(** Swapping between the customer's currencies at the provider's quoted price: a quote
    holds for a short time, executing it needs a fresh SCA (the action asks for a
    step-up when the API wants one), and the provider may ask for its own code. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

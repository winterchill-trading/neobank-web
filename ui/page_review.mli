(** The operators' review queue: payments the risk rules held. Two approvals by different
    operators release the payment (four eyes) to the customer's confirmation; rejecting
    cancels it. The live API wants the operator token, which this page keeps in memory
    only. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

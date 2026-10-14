(** Sending money: SEPA (standard or instant), on-chain crypto, or a Lightning invoice.
    The fee quote comes first; sending creates the payment and opens its page, where the
    customer confirms it (SCA). *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** One payment: its status and the confirmation it waits for. Our challenge takes the
    password and an authenticator code (bound to this amount and payee); a provider's
    challenge takes the code it sent. The page reloads every few seconds until the
    payment reaches a final state. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** Statuses after which nothing changes any more. *)
val is_final : string -> bool

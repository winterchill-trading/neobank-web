(** Opening an account: the person's details, then the authenticator secret to enrol
    (shown once, as a QR code), then the first sign-in. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

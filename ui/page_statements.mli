(** Statements for any account and period, with a CSV download built in the browser
    from the statement on screen. Going back more than 90 days needs a fresh SCA. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

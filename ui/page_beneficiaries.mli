(** Payees: SEPA accounts, crypto addresses and Lightning invoices the customer pays.
    Trusting one (which may exempt later payments from SCA) needs a fresh SCA. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** [(rail, label)] for the rails a payee can use. *)
val rails : (string * string) list

val rail_label : string -> string

(** The accounts list, and one account: its IBAN or deposit address with a QR code, its
    balance, the last 30 days, and a sandbox deposit to try the app with. *)

open! Core
open! Bonsai_web

val list_component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val detail_component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** What a deposit QR code carries: the IBAN, or a [bitcoin:]/[ethereum:] URI. *)
val qr_payload : Neobank_api_types.Api_types.Account.t -> string option

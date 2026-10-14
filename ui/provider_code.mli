(** Codes a provider sent by SMS or email for something it holds (a swap, a standing
    order, card details): one input per open challenge, answered with [POST /sca/:id]. *)

open! Core
open! Bonsai_web
open Neobank_api_types

(** [challenges] are [(what, challenge)]; nothing is drawn when the list is empty. *)
val view
  :  page:Page.t
  -> token:string
  -> bump:unit Effect.t
  -> action:Action.t
  -> form:Form.t
  -> set_form:(Form.t -> unit Effect.t)
  -> (string * Api_types.Sca_info.t) list
  -> Vdom.Node.t

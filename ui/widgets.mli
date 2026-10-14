(** Small view helpers shared by the pages. Class names refer to the stylesheet in
    [static/index.html]. *)

open! Core
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model

val link
  :  ?attrs:Vdom.Attr.t list
  -> Route.t
  -> Vdom.Node.t list
  -> Vdom.Node.t

val button
  :  ?id:string
  -> ?primary:bool
  -> ?danger:bool
  -> ?disabled:bool
  -> on_click:unit Effect.t
  -> string
  -> Vdom.Node.t

val failure : Api_failure.t -> Vdom.Node.t
val notice : ?kind:string -> string -> Vdom.Node.t
val loading : Vdom.Node.t

(** [Idle] renders nothing, [Loading] a line, [Failed] an error box. *)
val remote : 'a Remote.t -> f:('a -> Vdom.Node.t) -> Vdom.Node.t

val section : ?id:string -> string -> Vdom.Node.t list -> Vdom.Node.t
val stat : label:string -> string -> Vdom.Node.t
val stats : Vdom.Node.t list -> Vdom.Node.t

(** Header cells and rows of cells; [numeric] columns are right-aligned. *)
val table
  :  ?id:string
  -> header:string list
  -> numeric:int list
  -> Vdom.Node.t list list
  -> Vdom.Node.t

val amount : Api_types.Amount.t -> Vdom.Node.t

(** Green for incoming, red for outgoing. *)
val movement : direction:string -> Api_types.Amount.t -> Vdom.Node.t

val badge : string -> Vdom.Node.t

(** A label and a value on one line, the value in monospace with a copy hint. *)
val detail : label:string -> string -> Vdom.Node.t

(** The account's display name: ["EUR account"]. *)
val account_label : Api_types.Account.t -> string

(** A [<select>] of accounts, by id. *)
val account_select
  :  name:string
  -> label:string
  -> accounts:Api_types.Account.t list
  -> value:string
  -> on_change:(string -> unit Effect.t)
  -> Vdom.Node.t

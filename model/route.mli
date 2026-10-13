(** Pages of the app, addressed by the URL fragment so links and the back button work
    without a server-side router. *)

open! Core

type t =
  | Sign_in
  | Sign_up
  | Onboarding
  | Dashboard
  | Accounts
  | Account of string (** account id: details, deposit address, recent statement *)
  | Send
  | Transfer of string (** one transfer: status and its confirmation step *)
  | Swap
  | Cards
  | Standing_orders
  | Statements
  | Beneficiaries
  | Review (** operator review queue *)
[@@deriving compare, equal, sexp]

(** Accepts the fragment with or without the leading [#]. The empty fragment and anything
    unrecognised is [Dashboard]. *)
val of_hash : string -> t

(** ["#/dashboard"], ["#/accounts/acc_1"], ... *)
val to_hash : t -> string

val title : t -> string

(** Pages that need no session. *)
val is_public : t -> bool

(** The pages listed in the navigation bar, in order. *)
val menu : t list

(** The menu entry a page belongs to ([Account _] to [Accounts], ...). *)
val section : t -> t

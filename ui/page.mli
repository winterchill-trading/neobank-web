(** What every page component gets from the app. *)

open! Core
open! Bonsai_web

type t =
  { backend : Backend.t
  ; session : Session.t option Bonsai.t
  ; refresh : int Bonsai.t
  (** bumped after every change the app makes and on every push from the API, so pages
      load their data again *)
  ; bump : unit Effect.t Bonsai.t
  ; route : Neobank_web_model.Route.t Bonsai.t
  }

(** [Some (token, refresh)] while [active] and signed in: a key for {!Remote.fetch}. *)
val key : t -> active:bool Bonsai.t -> (string * int) option Bonsai.t

(** Whether the current route is [route]. *)
val showing : t -> (Neobank_web_model.Route.t -> bool) -> bool Bonsai.t

(** A page that needs a session shows this instead when signed out. *)
val signed_out : Vdom.Node.t

(** A button's request in flight and how it ended. When the API answers [sca_required]
    (the session's last strong authentication is older than 5 minutes), the status view
    asks for the password and an authenticator code, steps the session up, and tells the
    customer to press the button again. *)

open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_web_model

type status =
  | Idle
  | Busy of string
  | Done of string
  | Failed of Api_failure.t

type t =
  { status : status
  ; set : status -> unit Effect.t
  ; run :
      'a.
      busy:string
      -> (unit -> ('a, Api_failure.t) Result.t Deferred.t)
      -> on_ok:('a -> unit Effect.t)
      -> unit Effect.t
  ; view : Vdom.Node.t (** the status line, error, or step-up form *)
  }

val busy : t -> bool

val component
  :  backend:Backend.t
  -> session:Session.t option Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

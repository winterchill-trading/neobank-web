(** Data loaded from the backend for the current page. *)

open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_web_model

type 'a t =
  | Idle (** nothing requested: the page is not showing *)
  | Loading
  | Loaded of 'a
  | Failed of Api_failure.t

(** Calls [load] each time [key] changes to [Some k], and keeps the latest answer. While a
    new key loads, the answer for the previous key is not shown. Put a refresh counter in
    the key to load again after a change. *)
val fetch
  :  equal:('k -> 'k -> bool)
  -> load:('k -> ('a, Api_failure.t) Result.t Deferred.t)
  -> 'k option Bonsai.t
  -> local_ Bonsai.graph
  -> 'a t Bonsai.t

val to_option : 'a t -> 'a option

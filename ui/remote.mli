(** Data loaded over the network for the current page. *)

open! Core
open! Async_kernel
open! Bonsai_web

type 'a t =
  | Idle (** nothing requested: the page is not showing *)
  | Loading
  | Loaded of 'a
  | Failed of Error.t

(** Calls [load] each time [key] changes to [Some k], and keeps the latest answer. While a
    new key loads, the answer for the previous key is not shown; when the same key comes
    back the old answer stays on screen until the new one arrives. *)
val fetch
  :  equal:('k -> 'k -> bool)
  -> load:('k -> 'a Or_error.t Deferred.t)
  -> 'k option Bonsai.t
  -> local_ Bonsai.graph
  -> 'a t Bonsai.t

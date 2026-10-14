(** The few browser facilities the app uses outside the virtual DOM. *)

open! Core
open! Async_kernel

(** Milliseconds since the epoch, as a float (js_of_ocaml's [int] is 32 bits). *)
val now_ms : unit -> float

(** 16 random bytes from [crypto.getRandomValues], as hex: idempotency keys. *)
val random_id : unit -> string

(** Saves [contents] as a file through a temporary [blob:] link. *)
val download : file_name:string -> mime:string -> string -> unit

(** Adds a [<script src>] to the page once per URL; later calls wait on the first load.
    Fails if the script cannot be loaded (blocked, offline, wrong URL). *)
val load_script : string -> unit Or_error.t Deferred.t

(** [sessionStorage], ignoring browsers that refuse it (private modes). *)
val session_get : string -> string option

val session_set : string -> string option -> unit

(** [window.location] origin and path without query or fragment, e.g.
    ["http://localhost:8000/"]. *)
val page_base : unit -> string

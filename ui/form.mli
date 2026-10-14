(** Text fields kept in one map per form, so a page needs one piece of state for all of
    its inputs. *)

open! Core
open! Bonsai_web

type t = string String.Map.t [@@deriving sexp, equal]

val get : t -> string -> string

(** The state, and a setter for one field. *)
val state
  :  ?init:(string * string) list
  -> local_ Bonsai.graph
  -> t Bonsai.t * (t -> unit Effect.t) Bonsai.t

(** A labelled [<input>]; [name] is the field and the input's [name] attribute. [error]
    marks it invalid and shows the message under it. *)
val input
  :  ?type_:string
  -> ?placeholder:string
  -> ?error:string
  -> ?hint:string
  -> form:t
  -> set:(t -> unit Effect.t)
  -> name:string
  -> label:string
  -> unit
  -> Vdom.Node.t

(** A labelled [<select>] of [(value, label)] options. *)
val select
  :  form:t
  -> set:(t -> unit Effect.t)
  -> name:string
  -> label:string
  -> (string * string) list
  -> Vdom.Node.t

val checkbox
  :  form:t
  -> set:(t -> unit Effect.t)
  -> name:string
  -> label:string
  -> Vdom.Node.t

(** ["true"] when checked. *)
val checked : t -> string -> bool

(** The first error of a check, as a string for {!input}'s [error]. *)
val error_of : 'a Or_error.t -> string option

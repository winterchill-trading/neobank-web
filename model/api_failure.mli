(** Why a call to the API did not produce what was asked for. Pages show [message]; an
    [sca_required] answer carries the challenge to answer (a step-up). *)

open! Core
open Neobank_api_types

type t =
  { status : int (** HTTP status; 0 when the request never got an answer *)
  ; error : string (** the API's stable code, e.g. ["sca_required"], ["network"] *)
  ; message : string
  ; challenge : Api_types.Sca_info.t option
  }
[@@deriving sexp, compare, equal]

val create : ?challenge:Api_types.Sca_info.t -> status:int -> string -> string -> t

(** From a non-2xx answer: [Sca_required] or [Api_error] bodies, else the text itself. *)
val of_response : status:int -> string -> t

val network : string -> t

(** A 2xx body that did not decode. *)
val decode : Error.t -> t

val needs_step_up : t -> bool
val to_string : t -> string

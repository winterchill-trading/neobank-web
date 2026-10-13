(** QR codes (ISO/IEC 18004) for deposit addresses, IBANs and Lightning invoices: byte
    mode, the smallest version from 1 to 40 that fits, and the mask with the lowest
    penalty score. Written here rather than loaded from a script so the accounts page
    needs nothing from a third party, and so the encoder runs in the native tests. *)

open! Core

module Ecc : sig
  (** Error correction: roughly 7%, 15%, 25% and 30% of the symbol may be damaged. *)
  type t =
    | L
    | M
    | Q
    | H
  [@@deriving sexp, compare, equal]
end

type t

(** [ecc] defaults to [M]. Fails when the text does not fit version 40. *)
val encode : ?ecc:Ecc.t -> ?mask:int -> string -> t Or_error.t

val version : t -> int
val mask : t -> int

(** Modules per side: [17 + 4 * version]. *)
val size : t -> int

(** [true] for a dark module; [x] is the column, [y] the row, both from the top left. *)
val get : t -> x:int -> y:int -> bool

(** One SVG path ([d] attribute) drawing every dark module as a unit square, with
    horizontal runs merged, for a viewBox of [0 0 size size] (add a quiet zone of 4
    modules around it when drawing). *)
val svg_path : t -> string

(** Rows of ['#'] and ['.'], for tests. *)
val to_text : t -> string

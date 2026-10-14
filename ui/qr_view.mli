(** A QR code as inline SVG, with the 4-module quiet zone scanners need. *)

open! Core
open! Bonsai_web

(** Nothing (and a note) when the text is too long for a QR code. *)
val view : ?label:string -> string -> Vdom.Node.t

open! Core

type t =
  { token : string
  ; customer_id : string
  }
[@@deriving sexp, equal]

let key = "neobank-web.session"

let save t =
  Browser.session_set key (Option.map t ~f:(fun t -> Sexp.to_string (sexp_of_t t)))
;;

let restore () =
  Option.bind (Browser.session_get key) ~f:(fun s ->
    Option.try_with (fun () -> t_of_sexp (Sexp.of_string s)))
;;

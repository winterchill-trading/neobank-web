open! Core
open Neobank_api_types

type t =
  { status : int
  ; error : string
  ; message : string
  ; challenge : Api_types.Sca_info.t option
  }
[@@deriving sexp, compare, equal]

let create ?challenge ~status error message = { status; error; message; challenge }

let of_response ~status body =
  let json = Or_error.try_with (fun () -> Yojson.Safe.from_string body) in
  let try_decode f = Result.bind json ~f:(fun j -> Or_error.try_with (fun () -> f j)) in
  match try_decode Api_types.Sca_required.t_of_yojson with
  | Ok r -> create ~challenge:r.challenge ~status r.error r.message
  | Error _ ->
    (match try_decode Api_types.Api_error.t_of_yojson with
     | Ok e -> create ~status e.error e.message
     | Error _ ->
       let text = String.strip body |> fun s -> String.prefix s 200 in
       create
         ~status
         "http_error"
         (if String.is_empty text then sprintf "HTTP %d" status else text))
;;

let network message = create ~status:0 "network" message

let decode e =
  create ~status:200 "bad_response" ("unexpected answer: " ^ Error.to_string_hum e)
;;

let needs_step_up t = String.equal t.error "sca_required"

let to_string t =
  if t.status = 0 || String.is_empty t.error
  then t.message
  else sprintf "%s (%s)" t.message t.error
;;

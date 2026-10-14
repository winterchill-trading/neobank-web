open! Core
open! Bonsai_web
open Neobank_web_model
module N = Vdom.Node

type t =
  { backend : Backend.t
  ; session : Session.t option Bonsai.t
  ; refresh : int Bonsai.t
  ; bump : unit Effect.t Bonsai.t
  ; route : Route.t Bonsai.t
  }

let key t ~active =
  let open Bonsai.Let_syntax in
  let%map active
  and session = t.session
  and refresh = t.refresh in
  match active, (session : Session.t option) with
  | true, Some s -> Some (s.token, refresh)
  | _ -> None
;;

let showing t f =
  let open Bonsai.Let_syntax in
  let%map route = t.route in
  f route
;;

let signed_out =
  Widgets.section
    "Signed out"
    [ N.p
        [ N.text "Sign in to see this page. "
        ; Widgets.link Sign_in [ N.text "Sign in" ]
        ; N.text " or "
        ; Widgets.link Sign_up [ N.text "open an account" ]
        ; N.text "."
        ]
    ]
;;

open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

type status =
  | Idle
  | Busy of string
  | Done of string
  | Failed of Api_failure.t

type t =
  { status : status
  ; set : status -> unit Effect.t
  ; run :
      'a.
      busy:string
      -> (unit -> ('a, Api_failure.t) Result.t Deferred.t)
      -> on_ok:('a -> unit Effect.t)
      -> unit Effect.t
  ; view : Vdom.Node.t
  }

let busy t =
  match t.status with
  | Busy _ -> true
  | Idle | Done _ | Failed _ -> false
;;

let step_up_view ~(backend : Backend.t) ~session ~form ~set_form ~set =
  let password = Form.get form "password" in
  let code = Form.get form "totp" in
  let submit =
    match (session : Session.t option), Check.totp code with
    | None, _ | _, Error _ -> Effect.Ignore
    | Some s, Ok totp ->
      let open Effect.Let_syntax in
      let%bind () = set (Busy "Checking...") in
      (match%bind
         Effect.of_deferred_thunk (fun () ->
           backend.step_up ~token:s.token { password; totp })
       with
       | Ok _ ->
         let%bind () = set_form String.Map.empty in
         set (Done "Confirmed. Press the button again to continue.")
       | Error e -> set (Failed e))
  in
  N.div
    ~attrs:[ A.class_ "step-up"; A.id "step-up" ]
    [ N.p
        [ N.text
            "For your security, confirm it is you: enter your password and a code from \
             your authenticator app."
        ]
    ; N.div
        ~attrs:[ A.class_ "form-grid" ]
        [ Form.input
            ~type_:"password"
            ~form
            ~set:set_form
            ~name:"password"
            ~label:"Password"
            ()
        ; Form.input
            ~form
            ~set:set_form
            ~name:"totp"
            ~label:"Authenticator code"
            ~placeholder:"123456"
            ()
        ]
    ; Widgets.button ~id:"step-up-submit" ~primary:true ~on_click:submit "Confirm"
    ]
;;

let component ~backend ~session (local_ graph) =
  let open Bonsai.Let_syntax in
  let status, set = Bonsai.state Idle graph in
  let form, set_form = Form.state graph in
  let%map status and set and session and form and set_form in
  let run ~busy f ~on_ok =
    let open Effect.Let_syntax in
    let%bind () = set (Busy busy) in
    match%bind Effect.of_deferred_thunk f with
    | Ok x ->
      let%bind () = set Idle in
      on_ok x
    | Error e -> set (Failed e)
  in
  let view =
    match status with
    | Idle -> N.none
    | Busy text -> N.div ~attrs:[ A.class_ "loading" ] [ N.text text ]
    | Done text -> Widgets.notice ~kind:"success" text
    | Failed e when Api_failure.needs_step_up e ->
      step_up_view ~backend ~session ~form ~set_form ~set
    | Failed e -> Widgets.failure e
  in
  { status; set; run; view }
;;

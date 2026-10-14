open! Core
open! Bonsai_web
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let component (page : Page.t) ~login (local_ graph) =
  let open Bonsai.Let_syntax in
  let form, set_form = Form.state graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map form and set_form and action and login in
  let backend = page.backend in
  let email = Check.email (Form.get form "email") in
  let totp = Check.totp (Form.get form "totp") in
  let password = Form.get form "password" in
  let submit =
    match email, totp with
    | Ok email, Ok totp when not (String.is_empty password) ->
      action.run
        ~busy:"Signing in..."
        (fun () ->
          let open Async_kernel.Deferred.Result.Let_syntax in
          let%bind s = backend.login { email; password; totp } in
          let%map me = backend.me ~token:s.token in
          s, me)
        ~on_ok:(fun ((s : Neobank_api_types.Api_types.Auth.Session.t), me) ->
          let open Effect.Let_syntax in
          let%bind () = set_form String.Map.empty in
          let%bind () = login { Session.token = s.token; customer_id = s.customer_id } in
          Nav.navigate
            (if String.equal me.kyc.status "active" then Dashboard else Onboarding))
    | _ -> Effect.Ignore
  in
  let shown field result =
    if String.is_empty (Form.get form field) then None else Form.error_of result
  in
  Widgets.section
    "Sign in"
    [ (if backend.is_mock
       then
         Widgets.notice
           (sprintf
              "Mock data: sign in as %s with the password \"%s\" and any 6-digit code \
               (000000 is always wrong)."
              Mock_bank.demo_email
              Mock_bank.demo_password)
       else N.none)
    ; N.div
        ~attrs:[ A.class_ "form-grid" ]
        [ Form.input
            ~type_:"email"
            ~form
            ~set:set_form
            ~name:"email"
            ~label:"Email"
            ?error:(shown "email" email)
            ()
        ; Form.input
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
            ?error:(shown "totp" totp)
            ()
        ]
    ; N.div
        ~attrs:[ A.class_ "actions" ]
        [ Widgets.button
            ~id:"sign-in"
            ~primary:true
            ~disabled:
              (Action.busy action
               || Result.is_error email
               || Result.is_error totp
               || String.is_empty password)
            ~on_click:submit
            "Sign in"
        ; Widgets.link Sign_up [ N.text "Open an account" ]
        ]
    ; action.view
    ]
;;

open! Core
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let fields =
  [ "email", "Email", "email"
  ; "password", "Password (12 characters or more)", "password"
  ; "given_name", "Given name", "text"
  ; "family_name", "Family name", "text"
  ; "date_of_birth", "Date of birth (YYYY-MM-DD)", "text"
  ; "nationality", "Nationality (two letters)", "text"
  ; "mobile", "Mobile number", "tel"
  ; "line1", "Street and number", "text"
  ; "city", "City", "text"
  ; "postal_code", "Postal code", "text"
  ; "country", "Country of residence (two letters)", "text"
  ]
;;

let check form name =
  let v = Form.get form name in
  match name with
  | "email" -> Check.email v
  | "password" -> Check.password v
  | "date_of_birth" -> Check.date v
  | "nationality" | "country" -> Check.country v
  | "postal_code" -> Ok (String.strip v)
  | _ -> Check.required ~what:"This field" v
;;

let request form : Api_types.Auth.Signup_request.t Or_error.t =
  let open Or_error.Let_syntax in
  let get name = check form name in
  let%map email = get "email"
  and password = get "password"
  and given_name = get "given_name"
  and family_name = get "family_name"
  and date_of_birth = get "date_of_birth"
  and nationality = get "nationality"
  and mobile = get "mobile"
  and line1 = get "line1"
  and city = get "city"
  and postal_code = get "postal_code"
  and country = get "country" in
  { Api_types.Auth.Signup_request.email
  ; password
  ; given_name
  ; family_name
  ; date_of_birth
  ; nationality
  ; mobile
  ; address =
      { line1
      ; line2 = None
      ; city
      ; postal_code = (if String.is_empty postal_code then None else Some postal_code)
      ; country
      }
  }
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let form, set_form = Form.state graph in
  let created, set_created = Bonsai.state_opt graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map form and set_form and action and created and set_created in
  match (created : Api_types.Auth.Signup_response.t option) with
  | Some r ->
    Widgets.section
      ~id:"enrol"
      "Set up your authenticator"
      [ N.p
          [ N.text
              "Scan this code with an authenticator app (or type the key in by hand). \
               You need a code from it every time you sign in and to confirm payments. \
               It is shown only once."
          ]
      ; Qr_view.view ~label:"Authenticator setup" r.otpauth_uri
      ; Widgets.detail ~label:"Key" r.totp_secret
      ; N.div
          ~attrs:[ A.class_ "actions" ]
          [ Widgets.button
              ~id:"go-sign-in"
              ~primary:true
              ~on_click:
                (let open Effect.Let_syntax in
                 let%bind () = set_created None in
                 Nav.navigate Sign_in)
              "I have added it: sign in"
          ]
      ]
  | None ->
    let req = request form in
    let error name =
      if String.is_empty (Form.get form name)
      then None
      else Form.error_of (check form name)
    in
    let submit =
      match req with
      | Error _ -> Effect.Ignore
      | Ok req ->
        action.run
          ~busy:"Opening your account..."
          (fun () -> page.backend.signup req)
          ~on_ok:(fun r ->
            let open Effect.Let_syntax in
            let%bind () = set_form String.Map.empty in
            set_created (Some r))
    in
    Widgets.section
      "Open an account"
      [ N.p
          [ N.text
              "We need a few details before verifying your identity. They must match \
               your ID document."
          ]
      ; N.div
          ~attrs:[ A.class_ "form-grid" ]
          (List.map fields ~f:(fun (name, label, type_) ->
             Form.input ~type_ ~form ~set:set_form ~name ~label ?error:(error name) ()))
      ; N.div
          ~attrs:[ A.class_ "actions" ]
          [ Widgets.button
              ~id:"sign-up"
              ~primary:true
              ~disabled:(Action.busy action || Result.is_error req)
              ~on_click:submit
              "Open account"
          ; Widgets.link Sign_in [ N.text "I already have an account" ]
          ]
      ; action.view
      ]
;;

open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let step_label = function
  | "email" -> "Confirm your email address"
  | "mobile" -> "Confirm your mobile number"
  | "identity_document" -> "Photograph your ID document"
  | "liveness" -> "Take a short selfie video"
  | "address_proof" -> "Upload a proof of address"
  | "source_of_funds" -> "Tell us where your money comes from"
  | other -> Format.status other
;;

let status_text = function
  | "in_progress" -> "A few steps are left."
  | "action_required" -> "We need something from you: see the steps below."
  | "under_review" ->
    "Thank you. We are reviewing your details; this page updates by itself."
  | "active" -> "You are verified. Your accounts are ready."
  | "declined" -> "We could not verify your identity."
  | "closed" -> "This account is closed."
  | other -> Format.status other
;;

let sdk_container = "kyc-sdk"

(* The provider's SDK needs a fresh token when the first one runs out mid-flow; the API
   issues one for the same step. *)
let launch_sdk ~(backend : Backend.t) ~token ~(session : Api_types.Kyc.Session.t) ~bump =
  match session.sdk_token with
  | None -> return (Or_error.error_string "the API sent no SDK token for this step")
  | Some sdk_token ->
    Hosted_js.Kyc_sdk.launch
      ~container:sdk_container
      ~token:sdk_token
      ~refresh:(fun () ->
        match%map backend.kyc_start ~token { step = session.step } with
        | Ok { sdk_token = Some t; _ } -> Ok t
        | Ok { sdk_token = None; _ } -> Or_error.error_string "no new SDK token"
        | Error e -> Or_error.error_string (Api_failure.to_string e))
      ~on_message:(fun (_ : string) ->
        (* Any SDK event may mean the provider has news; reload the status. *)
        Effect.Expert.handle_non_dom_event_exn bump)
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Onboarding -> true
      | _ -> false)
  in
  let status =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) -> page.backend.kyc_status ~token)
      (Page.key page ~active)
      graph
  in
  let open_step, set_open_step = Bonsai.state_opt graph in
  let form, set_form = Form.state graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let sdk_error, set_sdk_error = Bonsai.state_opt graph in
  (* Results of hosted steps reach the API by webhook, so poll while one is open. *)
  let poll =
    let%map bump = page.bump
    and active
    and open_step in
    if active && Option.is_some open_step then bump else Effect.Ignore
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:false
    (Bonsai.return (Time_ns.Span.of_sec 5.))
    poll
    graph;
  let%map status
  and open_step
  and set_open_step
  and form
  and set_form
  and action
  and session = page.session
  and bump = page.bump
  and sdk_error
  and set_sdk_error in
  let backend = page.backend in
  match (session : Session.t option) with
  | None -> Page.signed_out
  | Some { token; _ } ->
    let start step =
      action.run
        ~busy:"Starting..."
        (fun () -> backend.kyc_start ~token { step })
        ~on_ok:(fun (s : Api_types.Kyc.Session.t) ->
          let open Effect.Let_syntax in
          let%bind () = set_open_step (Some s) in
          let%bind () = set_sdk_error None in
          if String.equal s.kind "hosted" && not backend.is_mock
          then (
            match%bind
              Effect.of_deferred_thunk (fun () ->
                launch_sdk ~backend ~token ~session:s ~bump)
            with
            | Ok () -> Effect.Ignore
            | Error e -> set_sdk_error (Some e))
          else Effect.Ignore)
    in
    let complete step code =
      action.run
        ~busy:"Checking..."
        (fun () -> backend.kyc_complete ~token { step; code })
        ~on_ok:(fun (_ : Api_types.Kyc.Status.t) ->
          let open Effect.Let_syntax in
          let%bind () = set_form String.Map.empty in
          let%bind () = set_open_step None in
          bump)
    in
    let open_view =
      match (open_step : Api_types.Kyc.Session.t option) with
      | None -> N.none
      | Some s when String.equal s.kind "code_sent" ->
        let code = Check.totp (Form.get form "code") in
        N.div
          ~attrs:[ A.class_ "step-open"; A.id "kyc-code" ]
          [ N.p
              [ N.text
                  (sprintf
                     "We sent you a code. It expires at %s."
                     (Format.time s.expires_at))
              ]
          ; Form.input
              ~form
              ~set:set_form
              ~name:"code"
              ~label:"Code"
              ~placeholder:"123456"
              ()
          ; Widgets.button
              ~id:"kyc-submit-code"
              ~primary:true
              ~disabled:(Result.is_error code || Action.busy action)
              ~on_click:
                (match code with
                 | Ok code -> complete s.step code
                 | Error _ -> Effect.Ignore)
              "Confirm"
          ]
      | Some s when backend.is_mock ->
        N.div
          ~attrs:[ A.class_ "step-open"; A.id "kyc-simulated" ]
          [ Widgets.notice
              "Mock data: the identity provider's SDK would open here. This stand-in \
               approves the step at once."
          ; N.div ~attrs:[ A.class_ "simulated-sdk" ] [ N.text (step_label s.step) ]
          ; Widgets.button
              ~id:"kyc-simulate"
              ~primary:true
              ~on_click:(complete s.step "simulated")
              "Finish the simulated check"
          ]
      | Some s ->
        N.div
          ~attrs:[ A.class_ "step-open" ]
          [ N.p
              [ N.text
                  (sprintf
                     "%s in the window below. The session expires at %s."
                     (step_label s.step)
                     (Format.time s.expires_at))
              ]
          ; (match sdk_error with
             | None -> N.none
             | Some e ->
               Widgets.notice
                 ~kind:"error"
                 ("The verification window could not start: " ^ Error.to_string_hum e))
          ]
    in
    Widgets.section
      "Verify your identity"
      [ Widgets.remote status ~f:(fun (st : Api_types.Kyc.Status.t) ->
          N.div
            [ N.p
                ~attrs:[ A.id "kyc-status" ]
                [ Widgets.badge st.status; N.text " "; N.text (status_text st.status) ]
            ; (if List.is_empty st.fields_to_resubmit
               then N.none
               else
                 Widgets.notice
                   ("Please correct: "
                    ^ String.concat
                        ~sep:", "
                        (List.map st.fields_to_resubmit ~f:Format.status)))
            ; (if String.equal st.status "active"
               then
                 Widgets.link
                   ~attrs:[ A.classes [ "btn"; "primary" ] ]
                   Dashboard
                   [ N.text "Go to your accounts" ]
               else
                 N.ol
                   ~attrs:[ A.class_ "steps" ]
                   (List.map st.steps_to_complete ~f:(fun step ->
                      N.li
                        [ N.span [ N.text (step_label step) ]
                        ; N.text " "
                        ; Widgets.button
                            ~id:("kyc-start-" ^ step)
                            ~disabled:(Action.busy action)
                            ~on_click:(start step)
                            "Start"
                        ])))
            ])
      ; open_view
      ; (* The SDK's iframe goes here; the element stays even when no step is open so a
           launch never races the render. *)
        Hosted_js.host ~class_:"sdk-host" sdk_container
      ; (match status with
         | Loaded st when not (String.equal st.status "active") ->
           N.div
             ~attrs:[ A.class_ "sandbox" ]
             [ N.h3 [ N.text "Sandbox" ]
             ; N.p
                 [ N.text
                     "In the sandbox the provider's decision can be simulated instead of \
                      waiting for its review."
                 ]
             ; Widgets.button
                 ~id:"kyc-sandbox-approve"
                 ~disabled:(Action.busy action)
                 ~on_click:
                   (action.run
                      ~busy:"Asking the sandbox to approve..."
                      (fun () -> backend.kyc_decision ~token { decision = "approve"; reason = None })
                      ~on_ok:(fun (_ : Api_types.Kyc.Status.t) ->
                        let open Effect.Let_syntax in
                        let%bind () = set_open_step None in
                        bump))
                 "Approve my verification"
             ]
         | _ -> N.none)
      ; action.view
      ]
;;

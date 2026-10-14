open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let is_final = function
  | "settled" | "rejected" | "failed" | "returned" | "cancelled" | "recalled" | "timed_out" ->
    true
  | _ -> false
;;

let explain (t : Api_types.Transfer.t) =
  match t.status with
  | "awaiting_sca" when Option.is_some t.sca -> "Confirm this payment to send it."
  | "awaiting_sca" | "submitted" when Option.is_some t.provider_sca ->
    "The bank sent you a code by SMS. Enter it to release the payment."
  | "under_review" ->
    "We are checking this payment before it can go out. You will be asked to confirm it \
     once it is released."
  | "submitted" | "pending" -> "On its way."
  | "settled" -> "Done."
  | "cancelled" -> "Cancelled; nothing was sent."
  | _ -> Option.value t.message ~default:""
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let transfer_id =
    let%map route = page.route in
    match (route : Route.t) with
    | Transfer id -> Some id
    | _ -> None
  in
  let key =
    let%map transfer_id and key = Page.key page ~active:(Bonsai.return true) in
    match transfer_id, key with
    | Some id, Some (token, n) -> Some (token, n, id)
    | _ -> None
  in
  let transfer =
    Remote.fetch
      ~equal:[%equal: string * int * string]
      ~load:(fun (token, _, id) -> page.backend.transfer ~token id)
      key
      graph
  in
  let poll =
    let%map transfer and bump = page.bump in
    match (transfer : Api_types.Transfer.t Remote.t) with
    | Loaded t when not (is_final t.status) -> bump
    | _ -> Effect.Ignore
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:false
    (Bonsai.return (Time_ns.Span.of_sec 3.))
    poll
    graph;
  let form, set_form = Form.state graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map transfer and form and set_form and action and session = page.session and bump = page.bump in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    Widgets.remote transfer ~f:(fun (t : Api_types.Transfer.t) ->
      let code = Check.totp (Form.get form "code") in
      let confirm ~password =
        match code with
        | Error _ -> Effect.Ignore
        | Ok code ->
          action.run
            ~busy:"Confirming..."
            (fun () -> page.backend.confirm_sca ~token t.id { code; password })
            ~on_ok:(fun _ ->
              let open Effect.Let_syntax in
              let%bind () = set_form String.Map.empty in
              bump)
      in
      let challenge_view =
        match t.sca, t.provider_sca with
        | Some (c : Api_types.Sca_info.t), _ ->
          let password = Form.get form "password" in
          N.div
            ~attrs:[ A.id "confirm-sca"; A.class_ "step-open" ]
            [ N.p
                [ N.text
                    (sprintf
                       "Confirm %s to %s with your password and a code from your \
                        authenticator app. %d attempts left; expires at %s."
                       (Format.amount t.amount)
                       t.counterparty
                       c.attempts_left
                       (Format.time c.expires_at))
                ]
            ; N.div
                ~attrs:[ A.class_ "form-grid" ]
                [ Form.input ~type_:"password" ~form ~set:set_form ~name:"password" ~label:"Password" ()
                ; Form.input ~form ~set:set_form ~name:"code" ~label:"Authenticator code" ~placeholder:"123456" ()
                ]
            ; N.div
                ~attrs:[ A.class_ "actions" ]
                [ Widgets.button
                    ~id:"confirm-submit"
                    ~primary:true
                    ~disabled:(Result.is_error code || String.is_empty password || Action.busy action)
                    ~on_click:(confirm ~password:(Some password))
                    "Confirm payment"
                ; Widgets.button
                    ~id:"restart-sca"
                    ~disabled:(Action.busy action)
                    ~on_click:
                      (action.run
                         ~busy:"Asking for a new challenge..."
                         (fun () -> page.backend.restart_sca ~token t.id)
                         ~on_ok:(fun _ -> bump))
                    "New challenge"
                ]
            ]
        | None, Some (c : Api_types.Sca_info.t) ->
          N.div
            ~attrs:[ A.id "confirm-provider"; A.class_ "step-open" ]
            [ N.p
                [ N.text
                    (sprintf "%d attempts left; expires at %s." c.attempts_left (Format.time c.expires_at))
                ]
            ; Form.input ~form ~set:set_form ~name:"code" ~label:"Code from the bank" ~placeholder:"123456" ()
            ; Widgets.button
                ~id:"provider-submit"
                ~primary:true
                ~disabled:(Result.is_error code || Action.busy action)
                ~on_click:(confirm ~password:None)
                "Release payment"
            ]
        | None, None -> N.none
      in
      Widgets.section
        ~id:"transfer"
        (sprintf "Payment to %s" t.counterparty)
        [ N.p [ Widgets.badge t.status; N.text " "; N.text (explain t) ]
        ; Widgets.stats
            [ Widgets.stat ~label:"Amount" (Format.amount t.amount)
            ; Widgets.stat ~label:"Fee" (Format.amount t.fee)
            ; Widgets.stat ~label:"Via" (Page_beneficiaries.rail_label t.rail)
            ; Widgets.stat ~label:"Created" (Format.time t.created_at)
            ]
        ; (if String.is_empty t.reference then N.none else Widgets.detail ~label:"Reference" t.reference)
        ; challenge_view
        ; (match t.status with
           | "awaiting_sca" | "under_review" | "draft" ->
             N.div
               ~attrs:[ A.class_ "actions" ]
               [ Widgets.button
                   ~id:"cancel-transfer"
                   ~danger:true
                   ~disabled:(Action.busy action)
                   ~on_click:
                     (action.run
                        ~busy:"Cancelling..."
                        (fun () -> page.backend.cancel_transfer ~token t.id)
                        ~on_ok:(fun _ -> bump))
                   "Cancel payment"
               ]
           | _ -> N.none)
        ; action.view
        ])
;;

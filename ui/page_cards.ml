open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

module Reveal = struct
  type stage =
    | Working of string
    | Code of Api_types.Sca_info.t
    | Shown of { simulated : string option }
    | Token_only
    | Failed of string

  type t =
    { card_id : string
    ; display : Hosted.Card_display.t option
    ; stage : stage
    }
end

let number_host id = "card-number-" ^ id
let cvv_host id = "card-cvv-" ^ id

let group_pan pan =
  String.to_list pan
  |> List.chunks_of ~length:4
  |> List.map ~f:String.of_list
  |> String.concat ~sep:" "
;;

(* Draws the number and CVV with the provider's plugin, or the mock's digits. *)
let show ~(backend : Backend.t) ~(card : Api_types.Card.t) ~display (token : Api_types.Card.Pan_token.t) =
  match (display : Hosted.Card_display.t option) with
  | Some d when String.equal d.provider "simulated" ->
    return (Reveal.Shown { simulated = backend.simulated_pan card.id })
  | None -> return Reveal.Token_only
  | Some d ->
    let render key dom_id =
      Hosted_js.Card_plugin.render
        ~key
        ~dom_id
        ~card_id:d.card_id
        ~auth_token:token.token
        ~masked:card.masked_pan
    in
    (* The plugin wants each render awaited before the next. *)
    (match%bind render "cardNumber" (number_host card.id) with
     | Error e -> return (Reveal.Failed (Error.to_string_hum e))
     | Ok () ->
       (match%map render "cvv" (cvv_host card.id) with
        | Error e -> Reveal.Failed (Error.to_string_hum e)
        | Ok () -> Reveal.Shown { simulated = None }))
;;

(* Starts the provider's consent: the plugin sends the code, the API learns the
   challenge. *)
let consent ~(backend : Backend.t) ~token ~(card : Api_types.Card.t) =
  match%bind backend.card_display ~token card.id with
  | Error e ->
    return
      ( None
      , Reveal.Failed
          ("This bank shows no card details here: " ^ Api_failure.to_string e) )
  | Ok d ->
    let%bind challenge_id =
      if String.equal d.provider "simulated"
      then return (Ok "simulated")
      else (
        match%bind Hosted_js.Card_plugin.create d with
        | Error e -> return (Error e)
        | Ok () -> Hosted_js.Card_plugin.request_consent ~user_id:d.user_id)
    in
    (match challenge_id with
     | Error e -> return (Some d, Reveal.Failed (Error.to_string_hum e))
     | Ok challenge_id ->
       (match%map backend.card_consent ~token card.id ~challenge_id with
        | Ok info -> Some d, Reveal.Code info
        | Error e -> Some d, Reveal.Failed (Api_failure.to_string e)))
;;

let reveal ~(backend : Backend.t) ~token ~(card : Api_types.Card.t) ~display =
  match%bind backend.pan_token ~token card.id with
  | Ok (`Challenge info) -> return (display, Reveal.Code info)
  | Ok (`Token t) ->
    (* A token from an earlier consent: the plugin still needs its settings. *)
    let%bind display =
      match display with
      | Some _ -> return display
      | None ->
        (match%map backend.card_display ~token card.id with
         | Ok d -> Some d
         | Error _ -> None)
    in
    (match display with
     | Some d when not (String.equal d.provider "simulated") ->
       (match%bind Hosted_js.Card_plugin.create d with
        | Error e -> return (display, Reveal.Failed (Error.to_string_hum e))
        | Ok () ->
          let%map stage = show ~backend ~card ~display t in
          display, stage)
     | _ ->
       let%map stage = show ~backend ~card ~display t in
       display, stage)
  | Error e when Api_failure.needs_step_up e -> return (display, Reveal.Failed "step-up")
  | Error _ when Option.is_none display -> consent ~backend ~token ~card
  | Error e -> return (display, Reveal.Failed (Api_failure.to_string e))
;;

let limits_form_init (l : Api_types.Card.Limits.t) =
  let amount = Option.value_map ~default:"" ~f:(fun (a : Api_types.Amount.t) -> a.amount) in
  [ "per_transaction", amount l.per_transaction
  ; "daily", amount l.daily
  ; "monthly", amount l.monthly
  ; "online", Bool.to_string l.online
  ; "contactless", Bool.to_string l.contactless
  ; "atm", Bool.to_string l.atm
  ]
;;

let limits_of_form form : Api_types.Card.Limits.t Or_error.t =
  let amount name =
    match String.strip (Form.get form name) with
    | "" -> Ok None
    | v ->
      Or_error.map (Units.parse_positive ~currency:"EUR" v) ~f:(fun z ->
        Some (Units.to_amount ~currency:"EUR" z))
  in
  let%map.Or_error per_transaction = amount "per_transaction"
  and daily = amount "daily"
  and monthly = amount "monthly" in
  { Api_types.Card.Limits.per_transaction
  ; daily
  ; monthly
  ; online = Form.checked form "online"
  ; contactless = Form.checked form "contactless"
  ; atm = Form.checked form "atm"
  }
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Cards -> true
      | _ -> false)
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) ->
        let open Deferred.Result.Let_syntax in
        let%bind cards = page.backend.cards ~token in
        let%map accounts = page.backend.accounts ~token in
        cards, accounts)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state graph in
  let editing, set_editing = Bonsai.state_opt graph in
  let reveal_state, set_reveal = Bonsai.state_opt graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map data and form and set_form and editing and set_editing and reveal_state and set_reveal
  and action
  and session = page.session
  and bump = page.bump in
  let backend = page.backend in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    let run_reveal (card : Api_types.Card.t) ~display =
      let open Effect.Let_syntax in
      let%bind () =
        set_reveal (Some { Reveal.card_id = card.id; display; stage = Working "Opening..." })
      in
      let%bind display, stage =
        Effect.of_deferred_thunk (fun () -> reveal ~backend ~token ~card ~display)
      in
      match stage with
      | Failed "step-up" ->
        let%bind () = set_reveal None in
        (* Let the action view ask for the password and code, then the customer clicks
           again. *)
        action.set
          (Failed
             (Api_failure.create ~status:403 "sca_required" "confirm it is you to see card details"))
      | stage -> set_reveal (Some { card_id = card.id; display; stage })
    in
    let reveal_view (card : Api_types.Card.t) =
      match (reveal_state : Reveal.t option) with
      | Some r when String.equal r.card_id card.id ->
        let hosts =
          (* Kept in the tree from the start so the plugin's iframes have somewhere to
             go. *)
          N.div
            ~attrs:[ A.class_ "card-secrets" ]
            [ Hosted_js.host ~class_:"secret" (number_host card.id)
            ; Hosted_js.host ~class_:"secret" (cvv_host card.id)
            ]
        in
        let body =
          match r.stage with
          | Working text -> N.div ~attrs:[ A.class_ "loading" ] [ N.text text ]
          | Code info ->
            let field = "pan-code" in
            let code = Check.totp (Form.get form field) in
            N.div
              ~attrs:[ A.id "pan-code"; A.class_ "step-open" ]
              [ N.p
                  [ N.text
                      (sprintf
                         "The bank sent you a code to show the card details (%d attempts left)."
                         info.attempts_left)
                  ]
              ; Form.input ~form ~set:set_form ~name:field ~label:"Code" ~placeholder:"123456" ()
              ; Widgets.button
                  ~id:"pan-code-submit"
                  ~primary:true
                  ~disabled:(Result.is_error code)
                  ~on_click:
                    (match code with
                     | Error _ -> Effect.Ignore
                     | Ok code ->
                       let open Effect.Let_syntax in
                       (match%bind
                          Effect.of_deferred_thunk (fun () ->
                            backend.provider_sca ~token info.challenge_id { code })
                        with
                        | Error e ->
                          set_reveal (Some { r with stage = Failed (Api_failure.to_string e) })
                        | Ok () ->
                          let%bind () = set_form (Map.remove form field) in
                          run_reveal card ~display:r.display))
                  "Show details"
              ]
          | Shown { simulated = Some pan } ->
            N.div
              ~attrs:[ A.id "pan-simulated"; A.class_ "simulated-card" ]
              [ Widgets.notice "Mock data: test digits, drawn here instead of by the provider's iframe."
              ; N.div ~attrs:[ A.class_ "mono pan" ] [ N.text (group_pan pan) ]
              ; N.div ~attrs:[ A.class_ "mono" ] [ N.text "CVV 123" ]
              ]
          | Shown { simulated = None } -> N.none
          | Token_only ->
            Widgets.notice
              "The bank issued a display token, but this provider has no card display \
               component to draw the details with."
          | Failed text -> Widgets.notice ~kind:"error" text
        in
        N.div [ body; hosts ]
      | _ -> N.none
    in
    let limits_view (card : Api_types.Card.t) =
      if not (Option.equal String.equal editing (Some card.id))
      then N.none
      else (
        let limits = limits_of_form form in
        N.div
          ~attrs:[ A.id ("limits-" ^ card.id); A.class_ "step-open" ]
          [ N.div
              ~attrs:[ A.class_ "form-grid" ]
              [ Form.input ~form ~set:set_form ~name:"per_transaction" ~label:"Per payment (EUR, empty for none)" ()
              ; Form.input ~form ~set:set_form ~name:"daily" ~label:"Per day (EUR)" ()
              ; Form.input ~form ~set:set_form ~name:"monthly" ~label:"Per month (EUR)" ()
              ]
          ; Form.checkbox ~form ~set:set_form ~name:"online" ~label:"Online payments"
          ; Form.checkbox ~form ~set:set_form ~name:"contactless" ~label:"Contactless"
          ; Form.checkbox ~form ~set:set_form ~name:"atm" ~label:"Cash machines"
          ; (match limits with
             | Error e -> Widgets.notice ~kind:"error" (Error.to_string_hum e)
             | Ok _ -> N.none)
          ; Widgets.button
              ~id:"limits-save"
              ~primary:true
              ~disabled:(Result.is_error limits || Action.busy action)
              ~on_click:
                (match limits with
                 | Error _ -> Effect.Ignore
                 | Ok l ->
                   action.run
                     ~busy:"Saving..."
                     (fun () -> backend.set_card_limits ~token card.id l)
                     ~on_ok:(fun _ ->
                       let open Effect.Let_syntax in
                       let%bind () = set_editing None in
                       let%bind () = action.set (Done "Limits saved.") in
                       bump))
              "Save limits"
          ])
    in
    let card_view (card : Api_types.Card.t) =
      let toggle =
        match card.status with
        | "active" ->
          Widgets.button
            ~id:("freeze-" ^ card.id)
            ~disabled:(Action.busy action)
            ~on_click:
              (action.run ~busy:"Freezing..." (fun () -> backend.freeze_card ~token card.id) ~on_ok:(fun _ -> bump))
            "Freeze"
        | "frozen" ->
          Widgets.button
            ~id:("unfreeze-" ^ card.id)
            ~disabled:(Action.busy action)
            ~on_click:
              (action.run ~busy:"Unfreezing..." (fun () -> backend.unfreeze_card ~token card.id) ~on_ok:(fun _ -> bump))
            "Unfreeze"
        | _ -> N.none
      in
      let l = card.limits in
      let limit (a : Api_types.Amount.t option) = Option.value_map a ~default:"none" ~f:Format.amount in
      N.div
        ~attrs:[ A.class_ "bank-card"; A.id ("card-" ^ card.id) ]
        [ N.div
            ~attrs:[ A.class_ "card-face" ]
            [ N.div ~attrs:[ A.class_ "mono pan" ] [ N.text (Option.value_map card.masked_pan ~default:"" ~f:group_pan) ]
            ; N.div [ Widgets.badge card.status; N.text (" " ^ Format.status card.kind) ]
            ]
        ; Widgets.stats
            [ Widgets.stat ~label:"Per payment" (limit l.per_transaction)
            ; Widgets.stat ~label:"Per day" (limit l.daily)
            ; Widgets.stat ~label:"Per month" (limit l.monthly)
            ; Widgets.stat
                ~label:"Allowed"
                (String.concat
                   ~sep:", "
                   (List.filter_opt
                      [ Option.some_if l.online "online"
                      ; Option.some_if l.contactless "contactless"
                      ; Option.some_if l.atm "cash"
                      ]))
            ]
        ; N.div
            ~attrs:[ A.class_ "actions" ]
            [ toggle
            ; Widgets.button
                ~id:("edit-limits-" ^ card.id)
                ~on_click:
                  (let open Effect.Let_syntax in
                   let%bind () = set_form (String.Map.of_alist_exn (limits_form_init card.limits)) in
                   set_editing (Some card.id))
                "Limits"
            ; Widgets.button
                ~id:("reveal-" ^ card.id)
                ~disabled:(not (String.equal card.status "active"))
                ~on_click:(run_reveal card ~display:None)
                "Show card details"
            ]
        ; limits_view card
        ; reveal_view card
        ]
    in
    Widgets.remote data ~f:(fun (cards, accounts) ->
      N.div
        [ Widgets.section
            ~id:"cards"
            "Cards"
            [ (if List.is_empty cards
               then Widgets.notice "No cards yet."
               else N.div ~attrs:[ A.class_ "cards" ] (List.map cards ~f:card_view))
            ; action.view
            ]
        ; Widgets.section
            ~id:"issue-card"
            "New virtual card"
            [ Widgets.account_select
                ~name:"card_account"
                ~label:"Pays from"
                ~accounts
                ~value:(Form.get form "card_account")
                ~on_change:(fun v -> set_form (Map.set form ~key:"card_account" ~data:v))
            ; Widgets.button
                ~id:"issue-card-submit"
                ~primary:true
                ~disabled:(String.is_empty (Form.get form "card_account") || Action.busy action)
                ~on_click:
                  (action.run
                     ~busy:"Issuing..."
                     (fun () -> backend.issue_card ~token { account_id = Form.get form "card_account" })
                     ~on_ok:(fun (_ : Api_types.Card.t) ->
                       let open Effect.Let_syntax in
                       let%bind () = action.set (Done "Your new card is ready.") in
                       bump))
                "Issue card"
            ]
        ])
;;

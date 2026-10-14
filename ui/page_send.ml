open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let kinds =
  [ "sepa", "Bank transfer (SEPA)"; "crypto", "Crypto"; "lightning", "Lightning" ]
;;

let account_fits ~kind (a : Api_types.Account.t) =
  match kind with
  | "sepa" -> String.equal a.currency "EUR"
  | "lightning" -> String.equal a.currency "BTC"
  | _ -> Currency.is_crypto a.currency
;;

(* The on-chain rail for an account: its own network, or the currency for native coins. *)
let onchain_rail (a : Api_types.Account.t) =
  "onchain:" ^ String.lowercase (Option.value a.network ~default:a.currency)
;;

let payee_fits ~kind ~(account : Api_types.Account.t option) (b : Api_types.Beneficiary.t)
  =
  let has r = List.mem b.rails r ~equal:String.equal in
  match kind with
  | "sepa" -> has "sepa" || has "sepa_instant"
  | "lightning" -> has "lightning"
  | _ ->
    (match account with
     | Some a -> has (onchain_rail a)
     | None -> List.exists b.rails ~f:(String.is_prefix ~prefix:"onchain:"))
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Send -> true
      | _ -> false)
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) ->
        let open Deferred.Result.Let_syntax in
        let%bind accounts = page.backend.accounts ~token in
        let%map payees = page.backend.beneficiaries ~token in
        accounts, payees)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state ~init:[ "kind", "sepa"; "rail", "sepa" ] graph in
  let quote, set_quote = Bonsai.state_opt graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map data
  and form
  and set_form
  and quote
  and set_quote
  and action
  and session = page.session in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    Widgets.remote data ~f:(fun (accounts, payees) ->
      let kind = Form.get form "kind" in
      let accounts = List.filter accounts ~f:(account_fits ~kind) in
      let account =
        List.find accounts ~f:(fun a -> String.equal a.id (Form.get form "account"))
      in
      let payees = List.filter payees ~f:(payee_fits ~kind ~account) in
      let payee =
        List.find payees ~f:(fun b -> String.equal b.id (Form.get form "payee"))
      in
      let rail =
        match kind, account with
        | "sepa", _ -> Form.get form "rail"
        | "lightning", _ -> "lightning"
        | _, Some a -> onchain_rail a
        | _, None -> ""
      in
      let amount =
        match account with
        | None -> Or_error.error_string "choose an account"
        | Some a -> Units.parse_positive ~currency:a.currency (Form.get form "amount")
      in
      (* Any edit invalidates the quote shown. *)
      let set_form f =
        let open Effect.Let_syntax in
        let%bind () = set_quote None in
        set_form f
      in
      let request : Api_types.Transfer.Request.t Or_error.t =
        match account, payee, amount with
        | Some a, Some b, Ok units ->
          Ok
            { account_id = a.id
            ; beneficiary_id = b.id
            ; amount = Units.to_amount ~currency:a.currency units
            ; rail = Some rail
            ; reference =
                (match String.strip (Form.get form "reference") with
                 | "" -> None
                 | r -> Some r)
            ; wallet_provider =
                (match kind, String.strip (Form.get form "wallet_provider") with
                 | "crypto", (_ as w) when not (String.is_empty w) -> Some w
                 | _ -> None)
            }
        | None, _, _ -> Or_error.error_string "choose an account"
        | _, None, _ -> Or_error.error_string "choose a payee"
        | _, _, Error e -> Error e
      in
      let get_quote =
        match request with
        | Error _ -> Effect.Ignore
        | Ok r ->
          action.run
            ~busy:"Asking for the fee..."
            (fun () ->
              page.backend.fee_quote
                ~token
                { account_id = r.account_id; rail; amount = r.amount })
            ~on_ok:(fun q -> set_quote (Some q))
      in
      let send =
        match request with
        | Error _ -> Effect.Ignore
        | Ok r ->
          action.run
            ~busy:"Creating the payment..."
            (fun () -> page.backend.create_transfer ~token r)
            ~on_ok:(fun (t : Api_types.Transfer.t) ->
              let open Effect.Let_syntax in
              let%bind () = set_quote None in
              let%bind () =
                set_form (String.Map.of_alist_exn [ "kind", kind; "rail", "sepa" ])
              in
              Nav.navigate (Transfer t.id))
      in
      Widgets.section
        ~id:"send"
        "Send money"
        [ N.div
            ~attrs:[ A.class_ "tabs" ]
            (List.map kinds ~f:(fun (k, label) ->
               N.button
                 ~attrs:
                   [ A.classes
                       ("tab" :: (if String.equal k kind then [ "active" ] else []))
                   ; A.id ("kind-" ^ k)
                   ; A.on_click (fun _ ->
                       set_form (String.Map.of_alist_exn [ "kind", k; "rail", "sepa" ]))
                   ]
                 [ N.text label ]))
        ; N.div
            ~attrs:[ A.class_ "form-grid" ]
            [ Widgets.account_select
                ~name:"account"
                ~label:"From"
                ~accounts
                ~value:(Form.get form "account")
                ~on_change:(fun v -> set_form (Map.set form ~key:"account" ~data:v))
            ; N.label
                ~attrs:[ A.class_ "field" ]
                [ N.span [ N.text "To" ]
                ; N.select
                    ~attrs:
                      [ A.name "payee"
                      ; A.on_change (fun _ v ->
                          set_form (Map.set form ~key:"payee" ~data:v))
                      ]
                    (N.option
                       ~attrs:[ A.value ""; A.disabled ]
                       [ N.text "Choose a payee" ]
                     :: List.map payees ~f:(fun (b : Api_types.Beneficiary.t) ->
                       N.option
                         ~attrs:
                           (A.value b.id
                            ::
                            (if Option.exists payee ~f:(fun p -> String.equal p.id b.id)
                             then [ A.selected ]
                             else []))
                         [ N.text b.name ]))
                ]
            ; (if String.equal kind "sepa"
               then
                 Form.select
                   ~form
                   ~set:set_form
                   ~name:"rail"
                   ~label:"Speed"
                   [ "sepa", "Standard (1 business day)"; "sepa_instant", "Instant" ]
               else N.none)
            ; Form.input
                ~form
                ~set:set_form
                ~name:"amount"
                ~label:
                  ("Amount"
                   ^ Option.value_map account ~default:"" ~f:(fun a ->
                     " in " ^ a.currency))
                ?error:
                  (if String.is_empty (Form.get form "amount") || Option.is_none account
                   then None
                   else Form.error_of amount)
                ()
            ; Form.input
                ~form
                ~set:set_form
                ~name:"reference"
                ~label:"Reference (optional)"
                ()
            ; (if String.equal kind "crypto"
               then
                 Form.input
                   ~form
                   ~set:set_form
                   ~name:"wallet_provider"
                   ~label:"Wallet provider of the payee (empty for a self-hosted wallet)"
                   ()
               else N.none)
            ]
        ; (if List.is_empty payees
           then
             N.p
               [ N.text "No payee for this kind of payment yet. "
               ; Widgets.link Beneficiaries [ N.text "Add one" ]
               ; N.text "."
               ]
           else N.none)
        ; (match (quote : Api_types.Fee_quote.t option) with
           | None -> N.none
           | Some q ->
             N.div
               ~attrs:[ A.id "quote" ]
               [ Widgets.stats
                   [ Widgets.stat ~label:"Amount" (Format.amount q.amount)
                   ; Widgets.stat ~label:"Fee" (Format.amount q.fee)
                   ; Widgets.stat ~label:"Total" (Format.amount q.total)
                   ; Widgets.stat ~label:"Via" (Page_beneficiaries.rail_label q.rail)
                   ]
               ])
        ; N.div
            ~attrs:[ A.class_ "actions" ]
            [ Widgets.button
                ~id:"get-quote"
                ~disabled:(Result.is_error request || Action.busy action)
                ~on_click:get_quote
                "Get fee quote"
            ; Widgets.button
                ~id:"send-submit"
                ~primary:true
                ~disabled:
                  (Option.is_none quote || Result.is_error request || Action.busy action)
                ~on_click:send
                "Continue to confirm"
            ]
        ; action.view
        ])
;;

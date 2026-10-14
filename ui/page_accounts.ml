open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let qr_payload (a : Api_types.Account.t) =
  match a.iban, a.address with
  | Some iban, _ -> Some iban
  | None, Some address ->
    (match a.currency, a.network with
     | "BTC", _ -> Some ("bitcoin:" ^ address)
     | "ETH", _ -> Some ("ethereum:" ^ address)
     | _ -> Some address)
  | None, None -> None
;;

let list_component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Accounts -> true
      | _ -> false)
  in
  let accounts =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) -> page.backend.accounts ~token)
      (Page.key page ~active)
      graph
  in
  let%map accounts
  and session = page.session in
  match session with
  | None -> Page.signed_out
  | Some _ ->
    Widgets.section
      "Accounts"
      [ Widgets.remote accounts ~f:(fun accounts ->
          if List.is_empty accounts
          then Widgets.notice "Your accounts open once your identity is verified."
          else
            Widgets.table
              ~id:"accounts"
              ~header:[ "Account"; "Details"; "Status"; "Available"; "On hold" ]
              ~numeric:[ 3; 4 ]
              (List.map accounts ~f:(fun (a : Api_types.Account.t) ->
                 [ Widgets.link (Account a.id) [ N.text (Widgets.account_label a) ]
                 ; N.span
                     ~attrs:[ A.class_ "mono" ]
                     [ N.text
                         (match a.iban, a.address with
                          | Some iban, _ -> Format.iban iban
                          | None, Some address -> Format.short address
                          | None, None -> "")
                     ]
                 ; Widgets.badge a.status
                 ; (match a.balance with
                    | Some b -> Widgets.amount b.available
                    | None -> N.text "-")
                 ; (match a.balance with
                    | Some b -> Widgets.amount b.held
                    | None -> N.text "-")
                 ])))
      ]
;;

let details_view (a : Api_types.Account.t) =
  let lines =
    List.filter_opt
      [ Option.map a.iban ~f:(fun i -> Widgets.detail ~label:"IBAN" (Format.iban i))
      ; Option.map a.bic ~f:(fun b -> Widgets.detail ~label:"BIC" b)
      ; Option.map a.address ~f:(fun x -> Widgets.detail ~label:"Deposit address" x)
      ; Option.map a.network ~f:(fun n -> Widgets.detail ~label:"Network" n)
      ; Option.map a.lightning_node ~f:(fun n -> Widgets.detail ~label:"Lightning node" n)
      ]
  in
  N.div
    ~attrs:[ A.class_ "account-details" ]
    [ N.div lines
    ; (match qr_payload a with
       | None -> N.none
       | Some payload ->
         Qr_view.view
           ~label:
             (if Option.is_some a.iban
              then "IBAN"
              else
                sprintf
                  "Send only %s on %s to this address"
                  a.currency
                  (Option.value a.network ~default:a.currency))
           payload)
    ]
;;

let deposit_view
  ~(action : Action.t)
  ~form
  ~set_form
  ~(a : Api_types.Account.t)
  ~token
  ~(backend : Backend.t)
  ~bump
  =
  let amount = Units.parse_positive ~currency:a.currency (Form.get form "deposit") in
  N.div
    ~attrs:[ A.class_ "sandbox" ]
    [ N.h3 [ N.text "Sandbox: simulate an incoming payment" ]
    ; N.div
        ~attrs:[ A.class_ "form-grid" ]
        [ Form.input
            ~form
            ~set:set_form
            ~name:"deposit"
            ~label:("Amount in " ^ a.currency)
            ?error:
              (if String.is_empty (Form.get form "deposit")
               then None
               else Form.error_of amount)
            ()
        ]
    ; Widgets.button
        ~id:"simulate-deposit"
        ~disabled:(Result.is_error amount || Action.busy action)
        ~on_click:
          (match amount with
           | Error _ -> Effect.Ignore
           | Ok units ->
             action.run
               ~busy:"Sending the simulated payment..."
               (fun () ->
                 backend.deposit
                   ~token
                   { account_id = a.id
                   ; amount = Units.to_amount ~currency:a.currency units
                   ; rail = None
                   })
               ~on_ok:(fun () ->
                 let open Effect.Let_syntax in
                 let%bind () = set_form String.Map.empty in
                 let%bind () =
                   action.set (Done "Deposit sent; it shows once the bank books it.")
                 in
                 bump))
        "Simulate deposit"
    ]
;;

let detail_component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let account_id =
    let%map route = page.route in
    match (route : Route.t) with
    | Account id -> Some id
    | _ -> None
  in
  let key =
    let%map account_id
    and key = Page.key page ~active:(Bonsai.return true) in
    match account_id, key with
    | Some id, Some (token, n) -> Some (token, n, id)
    | _ -> None
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int * string]
      ~load:(fun (token, _, id) ->
        let open Deferred.Result.Let_syntax in
        let%bind account = page.backend.account ~token id in
        let%map statement = page.backend.statement ~token id in
        account, statement)
      key
      graph
  in
  let form, set_form = Form.state graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map data
  and form
  and set_form
  and action
  and session = page.session
  and bump = page.bump in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    Widgets.remote
      data
      ~f:(fun ((a : Api_types.Account.t), (st : Api_types.Statement.t)) ->
        N.div
          [ Widgets.section
              ~id:"account"
              (Widgets.account_label a)
              [ (match a.balance with
                 | None -> Widgets.notice "The balance is not available right now."
                 | Some b ->
                   Widgets.stats
                     [ Widgets.stat ~label:"Available" (Format.amount b.available)
                     ; Widgets.stat ~label:"On hold" (Format.amount b.held)
                     ; Widgets.stat ~label:"Booked" (Format.amount b.posted)
                     ])
              ; details_view a
              ; deposit_view ~action ~form ~set_form ~a ~token ~backend:page.backend ~bump
              ; action.view
              ]
          ; Widgets.section
              "Last 30 days"
              [ (if List.is_empty st.lines
                 then Widgets.notice "No movements in this period."
                 else
                   Widgets.table
                     ~header:[ "Date"; "Description"; "Amount"; "Balance" ]
                     ~numeric:[ 2; 3 ]
                     (List.rev_map st.lines ~f:(fun (l : Api_types.Statement.Line.t) ->
                        [ N.text (Format.time l.at)
                        ; N.text
                            (if String.is_empty l.reference
                             then Format.status l.code
                             else l.reference)
                        ; N.text (Format.signed_amount l.amount)
                        ; N.text (Format.amount l.balance)
                        ])))
              ; Widgets.link Statements [ N.text "Statements and CSV" ]
              ]
          ])
;;

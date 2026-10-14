open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let activity (transfers : Api_types.Transfer.t list) =
  match transfers with
  | [] -> Widgets.notice "No payments yet."
  | _ ->
    Widgets.table
      ~id:"activity"
      ~header:[ "Date"; "Counterparty"; "Reference"; "Status"; "Amount" ]
      ~numeric:[ 4 ]
      (List.map transfers ~f:(fun (t : Api_types.Transfer.t) ->
         [ N.text (Format.date t.created_at)
         ; Widgets.link (Transfer t.id) [ N.text t.counterparty ]
         ; N.text t.reference
         ; Widgets.badge t.status
         ; Widgets.movement ~direction:t.direction t.amount
         ]))
;;

let balance_card (a : Api_types.Account.t) =
  N.a
    ~attrs:[ A.href (Route.to_hash (Account a.id)); A.class_ "balance"; A.id ("balance-" ^ a.currency) ]
    [ N.div ~attrs:[ A.class_ "balance-currency" ] [ N.text (Currency.get a.currency).name ]
    ; N.div
        ~attrs:[ A.class_ "balance-amount" ]
        [ N.text
            (Option.value_map a.balance ~default:"balance unavailable" ~f:(fun b ->
               Format.amount b.available))
        ]
    ; (match a.balance with
       | Some b
         when Result.value_map (Units.of_amount b.held) ~ok:(Fn.non (Z.equal Z.zero)) ~error:(Fn.const false) ->
         N.div ~attrs:[ A.class_ "muted" ] [ N.text ("on hold " ^ Format.amount b.held) ]
       | _ -> N.none)
    ]
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Dashboard -> true
      | _ -> false)
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) ->
        let open Deferred.Result.Let_syntax in
        let%bind me = page.backend.me ~token in
        let%bind accounts =
          if String.equal me.kyc.status "active" then page.backend.accounts ~token else return []
        in
        let%map transfers =
          if String.equal me.kyc.status "active" then page.backend.transfers ~token else return []
        in
        me, accounts, transfers)
      (Page.key page ~active)
      graph
  in
  let%map data and session = page.session in
  match session with
  | None -> Page.signed_out
  | Some _ ->
    Widgets.remote data ~f:(fun ((me : Api_types.Me.t), accounts, transfers) ->
      N.div
        [ (if String.equal me.kyc.status "active"
           then N.none
           else
             Widgets.section
               "Finish opening your account"
               [ N.p [ N.text "Verify your identity to get your IBAN, wallets and cards." ]
               ; Widgets.link ~attrs:[ A.classes [ "btn"; "primary" ] ] Onboarding [ N.text "Continue verification" ]
               ])
        ; (if List.is_empty accounts
           then N.none
           else
             Widgets.section
               "Balances"
               [ N.div ~attrs:[ A.class_ "balances" ] (List.map accounts ~f:balance_card) ])
        ; (if String.equal me.kyc.status "active"
           then
             Widgets.section
               "Recent activity"
               [ activity (List.take transfers 8)
               ; N.div
                   ~attrs:[ A.class_ "actions" ]
                   [ Widgets.link ~attrs:[ A.classes [ "btn"; "primary" ] ] Send [ N.text "Send money" ]
                   ; Widgets.link ~attrs:[ A.class_ "btn" ] Swap [ N.text "Swap" ]
                   ; Widgets.link ~attrs:[ A.class_ "btn" ] Statements [ N.text "Statements" ]
                   ]
               ]
           else N.none)
        ])
;;

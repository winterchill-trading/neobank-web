open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Swap -> true
      | _ -> false)
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) ->
        let open Deferred.Result.Let_syntax in
        let%bind accounts = page.backend.accounts ~token in
        let%map swaps = page.backend.swaps ~token in
        accounts, swaps)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state graph in
  let quote, set_quote = Bonsai.state_opt graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph in
  let%map data
  and form
  and set_form
  and quote
  and set_quote
  and action
  and session = page.session
  and bump = page.bump
  and now in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    Widgets.remote data ~f:(fun ((accounts : Api_types.Account.t list), swaps) ->
      let find id = List.find accounts ~f:(fun a -> String.equal a.id id) in
      let from = find (Form.get form "from") in
      let to_ = find (Form.get form "to") in
      let set_form f =
        let open Effect.Let_syntax in
        let%bind () = set_quote None in
        set_form f
      in
      let sell =
        match from with
        | None -> Or_error.error_string "choose what to sell"
        | Some a -> Units.parse_positive ~currency:a.currency (Form.get form "sell")
      in
      let request : Api_types.Swap.Quote_request.t Or_error.t =
        match from, to_, sell with
        | Some f, Some t, Ok units when not (String.equal f.id t.id) ->
          Ok
            { from_account_id = f.id
            ; to_account_id = t.id
            ; sell = Units.to_amount ~currency:f.currency units
            }
        | Some f, Some t, Ok _ when String.equal f.id t.id ->
          Or_error.error_string "pick two different accounts"
        | _, _, Error e -> Error e
        | _ -> Or_error.error_string "choose both accounts"
      in
      let seconds_left (q : Api_types.Swap.t) =
        match Time_ns.of_string_with_utc_offset q.expires_at with
        | exception _ -> None
        | t -> Some (Time_ns.Span.to_sec (Time_ns.diff t now) |> Float.iround_down_exn)
      in
      let quote_view =
        match (quote : Api_types.Swap.t option) with
        | None -> N.none
        | Some q ->
          let left = seconds_left q in
          let expired = Option.exists left ~f:(fun s -> s <= 0) in
          N.div
            ~attrs:[ A.id "swap-quote" ]
            [ Widgets.stats
                [ Widgets.stat ~label:"You sell" (Format.amount q.sell)
                ; Widgets.stat ~label:"Fee" (Format.amount q.fee)
                ; Widgets.stat ~label:"You get" (Format.amount q.buy)
                ; Widgets.stat
                    ~label:"Price"
                    (q.price ^ " " ^ q.buy.currency ^ " per " ^ q.sell.currency)
                ; Widgets.stat
                    ~label:"Valid for"
                    (match left with
                     | None -> "-"
                     | Some s when s <= 0 -> "expired"
                     | Some s -> sprintf "%d s" s)
                ]
            ; Widgets.button
                ~id:"swap-execute"
                ~primary:true
                ~disabled:(expired || Action.busy action)
                ~on_click:
                  (action.run
                     ~busy:"Swapping..."
                     (fun () -> page.backend.execute_swap ~token q.id)
                     ~on_ok:(fun (s : Api_types.Swap.t) ->
                       let open Effect.Let_syntax in
                       let%bind () = set_quote None in
                       let%bind () =
                         action.set
                           (Done
                              (match s.status with
                               | "executed" -> "Swapped."
                               | "awaiting_sca" ->
                                 "The bank sent you a code to confirm the swap; see \
                                  below."
                               | _ -> "Swap sent; the balances update when it completes."))
                       in
                       bump))
                "Swap now"
            ]
      in
      let pending =
        List.filter swaps ~f:(fun (s : Api_types.Swap.t) -> Option.is_some s.provider_sca)
      in
      N.div
        [ Widgets.section
            ~id:"swap"
            "Swap"
            [ N.div
                ~attrs:[ A.class_ "form-grid" ]
                [ Widgets.account_select
                    ~name:"from"
                    ~label:"Sell from"
                    ~accounts
                    ~value:(Form.get form "from")
                    ~on_change:(fun v -> set_form (Map.set form ~key:"from" ~data:v))
                ; Widgets.account_select
                    ~name:"to"
                    ~label:"Buy into"
                    ~accounts
                    ~value:(Form.get form "to")
                    ~on_change:(fun v -> set_form (Map.set form ~key:"to" ~data:v))
                ; Form.input
                    ~form
                    ~set:set_form
                    ~name:"sell"
                    ~label:
                      ("Amount to sell"
                       ^ Option.value_map from ~default:"" ~f:(fun a ->
                         " in " ^ a.currency))
                    ?error:
                      (if String.is_empty (Form.get form "sell")
                       then None
                       else Form.error_of request)
                    ()
                ]
            ; Widgets.button
                ~id:"swap-quote-button"
                ~disabled:(Result.is_error request || Action.busy action)
                ~on_click:
                  (match request with
                   | Error _ -> Effect.Ignore
                   | Ok r ->
                     action.run
                       ~busy:"Getting a price..."
                       (fun () -> page.backend.swap_quote ~token r)
                       ~on_ok:(fun q -> set_quote (Some q)))
                "Get a price"
            ; quote_view
            ; action.view
            ]
        ; Provider_code.view
            ~page
            ~token
            ~bump
            ~action
            ~form
            ~set_form
            (List.map pending ~f:(fun (s : Api_types.Swap.t) ->
               ( sprintf "Swap of %s" (Format.amount s.sell)
               , Option.value_exn s.provider_sca )))
        ; Widgets.section
            "Past swaps"
            [ (if List.is_empty swaps
               then Widgets.notice "No swaps yet."
               else
                 Widgets.table
                   ~id:"swaps"
                   ~header:[ "Sold"; "Bought"; "Fee"; "Status" ]
                   ~numeric:[ 0; 1; 2 ]
                   (List.map swaps ~f:(fun (s : Api_types.Swap.t) ->
                      [ N.text (Format.amount s.sell)
                      ; N.text (Format.amount s.buy)
                      ; N.text (Format.amount s.fee)
                      ; Widgets.badge s.status
                      ])))
            ]
        ])
;;

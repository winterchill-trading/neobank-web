open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let days = [ "mon"; "tue"; "wed"; "thu"; "fri"; "sat"; "sun" ]

let schedule_text s =
  match String.lsplit2 s ~on:':' with
  | None when String.equal s "daily" -> "Every day"
  | Some ("weekly", d) -> "Weekly on " ^ String.capitalize d
  | Some ("monthly", d) -> "Monthly on day " ^ d
  | _ -> s
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Standing_orders -> true
      | _ -> false)
  in
  let data =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) ->
        let open Deferred.Result.Let_syntax in
        let%bind orders = page.backend.standing_orders ~token in
        let%bind accounts = page.backend.accounts ~token in
        let%map payees = page.backend.beneficiaries ~token in
        orders, accounts, payees)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state ~init:[ "frequency", "monthly"; "day", "1"; "weekday", "mon" ] graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map data and form and set_form and action and session = page.session and bump = page.bump in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    Widgets.remote data ~f:(fun ((orders : Api_types.Standing_order.t list), accounts, payees) ->
      let payee_name id =
        List.find payees ~f:(fun (b : Api_types.Beneficiary.t) -> String.equal b.id id)
        |> Option.value_map ~default:id ~f:(fun b -> b.name)
      in
      let accounts = List.filter accounts ~f:(fun a -> String.equal a.currency "EUR") in
      let payees =
        List.filter payees ~f:(fun b ->
          List.exists b.rails ~f:(fun r -> String.is_prefix r ~prefix:"sepa"))
      in
      let schedule =
        match Form.get form "frequency" with
        | "daily" -> Ok "daily"
        | "weekly" -> Ok ("weekly:" ^ Form.get form "weekday")
        | _ ->
          (match Int.of_string_opt (String.strip (Form.get form "day")) with
           | Some d when d >= 1 && d <= 28 -> Ok (sprintf "monthly:%d" d)
           | _ -> Or_error.error_string "a day of the month from 1 to 28")
      in
      let request : Api_types.Standing_order.Request.t Or_error.t =
        let open Or_error.Let_syntax in
        let%bind account_id =
          match Form.get form "account" with
          | "" -> Or_error.error_string "choose an account"
          | a -> Ok a
        in
        let%bind beneficiary_id =
          match Form.get form "payee" with
          | "" -> Or_error.error_string "choose a payee"
          | b -> Ok b
        in
        let%bind amount = Units.parse_positive ~currency:"EUR" (Form.get form "amount") in
        let%bind start = Check.date (Form.get form "start") in
        let%bind until =
          match String.strip (Form.get form "until") with
          | "" -> Ok None
          | u -> Or_error.map (Check.date u) ~f:Option.some
        in
        let%map schedule in
        { Api_types.Standing_order.Request.account_id
        ; beneficiary_id
        ; amount = Units.to_amount ~currency:"EUR" amount
        ; schedule
        ; start
        ; until
        ; reference =
            (match String.strip (Form.get form "reference") with
             | "" -> None
             | r -> Some r)
        }
      in
      let pending =
        List.filter_map orders ~f:(fun (o : Api_types.Standing_order.t) ->
          Option.map o.provider_sca ~f:(fun c ->
            sprintf "Standing order to %s" (payee_name o.beneficiary_id), c))
      in
      N.div
        [ Widgets.section
            "Standing orders"
            [ (if List.is_empty orders
               then Widgets.notice "No standing orders."
               else
                 Widgets.table
                   ~id:"standing-orders"
                   ~header:[ "To"; "Amount"; "Schedule"; "Next"; "Runs"; "Status"; "" ]
                   ~numeric:[ 1; 4 ]
                   (List.map orders ~f:(fun (o : Api_types.Standing_order.t) ->
                      [ N.text (payee_name o.beneficiary_id)
                      ; N.text (Format.amount o.amount)
                      ; N.text (schedule_text o.schedule)
                      ; N.text o.next_run
                      ; N.textf "%d" o.runs
                      ; Widgets.badge o.status
                      ; (match o.status with
                         | "cancelled" | "completed" -> N.none
                         | _ ->
                           Widgets.button
                             ~id:("cancel-" ^ o.id)
                             ~danger:true
                             ~disabled:(Action.busy action)
                             ~on_click:
                               (action.run
                                  ~busy:"Cancelling..."
                                  (fun () -> page.backend.cancel_standing_order ~token o.id)
                                  ~on_ok:(fun _ -> bump))
                             "Cancel")
                      ])))
            ]
        ; Provider_code.view ~page ~token ~bump ~action ~form ~set_form pending
        ; Widgets.section
            ~id:"new-standing-order"
            "New standing order"
            [ N.div
                ~attrs:[ A.class_ "form-grid" ]
                [ Widgets.account_select
                    ~name:"account"
                    ~label:"From"
                    ~accounts
                    ~value:(Form.get form "account")
                    ~on_change:(fun v -> set_form (Map.set form ~key:"account" ~data:v))
                ; Form.select
                    ~form
                    ~set:set_form
                    ~name:"payee"
                    ~label:"To"
                    (("", "Choose a payee")
                     :: List.map payees ~f:(fun (b : Api_types.Beneficiary.t) -> b.id, b.name))
                ; Form.input ~form ~set:set_form ~name:"amount" ~label:"Amount in EUR" ()
                ; Form.select
                    ~form
                    ~set:set_form
                    ~name:"frequency"
                    ~label:"How often"
                    [ "daily", "Every day"; "weekly", "Every week"; "monthly", "Every month" ]
                ; (match Form.get form "frequency" with
                   | "weekly" ->
                     Form.select ~form ~set:set_form ~name:"weekday" ~label:"On" (List.map days ~f:(fun d -> d, String.capitalize d))
                   | "monthly" ->
                     Form.input ~form ~set:set_form ~name:"day" ~label:"Day of the month (1 to 28)" ()
                   | _ -> N.none)
                ; Form.input ~form ~set:set_form ~name:"start" ~label:"First payment (YYYY-MM-DD)" ()
                ; Form.input ~form ~set:set_form ~name:"until" ~label:"Last payment (optional)" ()
                ; Form.input ~form ~set:set_form ~name:"reference" ~label:"Reference (optional)" ()
                ]
            ; (match request with
               | Error e when not (String.is_empty (Form.get form "amount")) ->
                 Widgets.notice ~kind:"field-error" (Error.to_string_hum e)
               | _ -> N.none)
            ; Widgets.button
                ~id:"standing-order-submit"
                ~primary:true
                ~disabled:(Result.is_error request || Action.busy action)
                ~on_click:
                  (match request with
                   | Error _ -> Effect.Ignore
                   | Ok r ->
                     action.run
                       ~busy:"Setting it up..."
                       (fun () -> page.backend.create_standing_order ~token r)
                       ~on_ok:(fun (o : Api_types.Standing_order.t) ->
                         let open Effect.Let_syntax in
                         let%bind () =
                           action.set
                             (Done
                                (if Option.is_some o.provider_sca
                                 then "Almost done: enter the code the bank sent you."
                                 else "Standing order set up."))
                         in
                         bump))
                "Set up"
            ; action.view
            ]
        ])
;;

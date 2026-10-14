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
      | Statements -> true
      | _ -> false)
  in
  let accounts =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) -> page.backend.accounts ~token)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state graph in
  let statement, set_statement = Bonsai.state_opt graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map accounts and form and set_form and statement and set_statement and action
  and session = page.session in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    let date name =
      match String.strip (Form.get form name) with
      | "" -> Ok None
      | d -> Or_error.map (Check.date d) ~f:Option.some
    in
    let from = date "from" in
    let until = date "until" in
    let account_id = Form.get form "account" in
    let request =
      match from, until with
      | Ok from, Ok until when not (String.is_empty account_id) -> Ok (from, until)
      | Error e, _ | _, Error e -> Error e
      | _ -> Or_error.error_string "choose an account"
    in
    let load =
      match request with
      | Error _ -> Effect.Ignore
      | Ok (from, until) ->
        action.run
          ~busy:"Loading the statement..."
          (fun () -> page.backend.statement ~token ?from ?until account_id)
          ~on_ok:(fun s -> set_statement (Some s))
    in
    Widgets.section
      ~id:"statements"
      "Statements"
      [ Widgets.remote accounts ~f:(fun accounts ->
          N.div
            ~attrs:[ A.class_ "form-grid" ]
            [ Widgets.account_select
                ~name:"account"
                ~label:"Account"
                ~accounts
                ~value:account_id
                ~on_change:(fun v -> set_form (Map.set form ~key:"account" ~data:v))
            ; Form.input
                ~form
                ~set:set_form
                ~name:"from"
                ~label:"From (YYYY-MM-DD, default 30 days ago)"
                ?error:(Form.error_of from)
                ()
            ; Form.input
                ~form
                ~set:set_form
                ~name:"until"
                ~label:"Until (default today)"
                ?error:(Form.error_of until)
                ()
            ])
      ; Widgets.button
          ~id:"statement-load"
          ~primary:true
          ~disabled:(Result.is_error request || Action.busy action)
          ~on_click:load
          "Show statement"
      ; action.view
      ; (match (statement : Api_types.Statement.t option) with
         | None -> N.none
         | Some st ->
           N.div
             ~attrs:[ A.id "statement" ]
             [ Widgets.stats
                 [ Widgets.stat ~label:"Period" (Format.date st.from ^ " to " ^ Format.date st.until)
                 ; Widgets.stat ~label:"Opening" (Format.amount st.opening)
                 ; Widgets.stat ~label:"Closing" (Format.amount st.closing)
                 ; Widgets.stat ~label:"Movements" (Int.to_string (List.length st.lines))
                 ]
             ; (if List.is_empty st.lines
                then Widgets.notice "No movements in this period."
                else
                  Widgets.table
                    ~header:[ "Date"; "Type"; "Reference"; "Amount"; "Balance" ]
                    ~numeric:[ 3; 4 ]
                    (List.map st.lines ~f:(fun (l : Api_types.Statement.Line.t) ->
                       [ N.text (Format.time l.at)
                       ; N.text (Format.status l.code)
                       ; N.text l.reference
                       ; N.text (Format.signed_amount l.amount)
                       ; N.text (Format.amount l.balance)
                       ])))
             ; Widgets.button
                 ~id:"statement-csv"
                 ~on_click:
                   (Effect.of_thunk (fun () ->
                      Browser.download
                        ~file_name:(Statement_csv.file_name st)
                        ~mime:"text/csv"
                        (Statement_csv.to_string st)))
                 "Download CSV"
             ])
      ]
;;

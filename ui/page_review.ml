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
      | Review -> true
      | _ -> false)
  in
  let form, set_form = Form.state ~init:[ "operator", "operator" ] graph in
  let ops_token, set_ops_token = Bonsai.state_opt graph in
  let key =
    let%map active and ops_token and refresh = page.refresh in
    match active, ops_token with
    | true, Some t -> Some (t, refresh)
    | _ -> None
  in
  let reviews =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (ops_token, _) -> page.backend.reviews ~ops_token)
      key
      graph
  in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map reviews and form and set_form and ops_token and set_ops_token and action
  and bump = page.bump in
  let backend = page.backend in
  match ops_token with
  | None ->
    Widgets.section
      ~id:"review-login"
      "Review queue"
      [ (if backend.is_mock
         then Widgets.notice "Mock data: any operator token works."
         else N.none)
      ; Form.input
          ~type_:"password"
          ~form
          ~set:set_form
          ~name:"ops_token"
          ~label:"Operator token"
          ()
      ; Widgets.button
          ~id:"review-open"
          ~primary:true
          ~disabled:(String.is_empty (Form.get form "ops_token"))
          ~on_click:(set_ops_token (Some (Form.get form "ops_token")))
          "Open queue"
      ]
  | Some ops_token ->
    let decide (r : Api_types.Review.t) verdict =
      action.run
        ~busy:"Recording the decision..."
        (fun () ->
          backend.decide_review
            ~ops_token
            r.id
            { verdict
            ; operator = Form.get form "operator"
            ; note =
                (match String.strip (Form.get form ("note-" ^ r.id)) with
                 | "" -> None
                 | n -> Some n)
            })
        ~on_ok:(fun _ -> bump)
    in
    Widgets.section
      ~id:"review"
      "Review queue"
      [ Form.input ~form ~set:set_form ~name:"operator" ~label:"Your operator name" ()
      ; Widgets.remote reviews ~f:(fun reviews ->
          let pending = List.filter reviews ~f:(fun r -> String.equal r.status "pending") in
          if List.is_empty pending
          then Widgets.notice "Nothing to review."
          else
            N.div
              (List.map pending ~f:(fun (r : Api_types.Review.t) ->
                 N.div
                   ~attrs:[ A.class_ "review-item"; A.id ("review-" ^ r.id) ]
                   [ N.div
                       [ Widgets.badge r.severity
                       ; N.text (sprintf " Payment %s by customer %s, held %s" r.reference r.customer_id (Format.time r.submitted_at))
                       ]
                   ; N.ul (List.map r.reasons ~f:(fun reason -> N.li [ N.text reason ]))
                   ; Form.input ~form ~set:set_form ~name:("note-" ^ r.id) ~label:"Note (optional)" ()
                   ; N.div
                       ~attrs:[ A.class_ "actions" ]
                       [ Widgets.button
                           ~id:("approve-" ^ r.id)
                           ~primary:true
                           ~disabled:(Action.busy action || String.is_empty (Form.get form "operator"))
                           ~on_click:(decide r "approve")
                           "Approve"
                       ; Widgets.button
                           ~id:("reject-" ^ r.id)
                           ~danger:true
                           ~disabled:(Action.busy action || String.is_empty (Form.get form "operator"))
                           ~on_click:(decide r "reject")
                           "Reject"
                       ]
                   ])))
      ; action.view
      ; Widgets.button ~on_click:(set_ops_token None) "Close queue"
      ]
;;

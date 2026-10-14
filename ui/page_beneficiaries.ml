open! Core
open! Async_kernel
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let rails =
  [ "sepa", "SEPA transfer"
  ; "sepa_instant", "SEPA Instant"
  ; "onchain:btc", "Bitcoin"
  ; "onchain:eth", "Ethereum"
  ; "lightning", "Lightning"
  ]
;;

let rail_label r = List.Assoc.find rails r ~equal:String.equal |> Option.value ~default:r

let check_destination ~rail text =
  match rail with
  | "sepa" | "sepa_instant" -> Check.iban text
  | "lightning" -> Check.address ~network:"LIGHTNING" text
  | r ->
    (match String.chop_prefix r ~prefix:"onchain:" with
     | Some network -> Check.address ~network:(String.uppercase network) text
     | None -> Or_error.error_string "pick a payment type")
;;

let component (page : Page.t) (local_ graph) =
  let open Bonsai.Let_syntax in
  let active =
    Page.showing page (function
      | Beneficiaries -> true
      | _ -> false)
  in
  let list =
    Remote.fetch
      ~equal:[%equal: string * int]
      ~load:(fun (token, _) -> page.backend.beneficiaries ~token)
      (Page.key page ~active)
      graph
  in
  let form, set_form = Form.state ~init:[ "rail", "sepa" ] graph in
  let action = Action.component ~backend:page.backend ~session:page.session graph in
  let%map list
  and form
  and set_form
  and action
  and session = page.session
  and bump = page.bump in
  match session with
  | None -> Page.signed_out
  | Some { token; _ } ->
    let rail = Form.get form "rail" in
    let is_sepa = String.is_prefix rail ~prefix:"sepa" in
    let name = Check.required ~what:"Name" (Form.get form "name") in
    let destination = check_destination ~rail (Form.get form "destination") in
    let bic =
      match String.strip (Form.get form "bic") with
      | "" -> Ok None
      | b -> Or_error.map (Check.bic b) ~f:Option.some
    in
    let request =
      let%map.Or_error name and destination and bic in
      { Api_types.Beneficiary.Request.name
      ; rail
      ; destination
      ; bic = (if is_sepa then bic else None)
      ; memo = None
      }
    in
    let shown field r =
      if String.is_empty (Form.get form field) then None else Form.error_of r
    in
    let trust (b : Api_types.Beneficiary.t) =
      Widgets.button
        ~id:("trust-" ^ b.id)
        ~disabled:(Action.busy action)
        ~on_click:
          (action.run
             ~busy:"Trusting..."
             (fun () -> page.backend.trust_beneficiary ~token b.id)
             ~on_ok:(fun _ -> bump))
        "Trust"
    in
    N.div
      [ Widgets.section
          "Payees"
          [ Widgets.remote list ~f:(fun l ->
              if List.is_empty l
              then Widgets.notice "No payees yet."
              else
                Widgets.table
                  ~id:"payees"
                  ~header:[ "Name"; "Pays to"; "Type"; "Name check"; "Trusted" ]
                  ~numeric:[]
                  (List.map l ~f:(fun (b : Api_types.Beneficiary.t) ->
                     [ N.text b.name
                     ; N.span
                         ~attrs:[ A.class_ "mono" ]
                         [ N.text
                             (if Result.is_ok (Check.iban b.destination)
                              then Format.iban b.destination
                              else Format.short ~keep:10 b.destination)
                         ]
                     ; N.text (String.concat ~sep:", " (List.map b.rails ~f:rail_label))
                     ; N.text
                         (Option.value_map b.payee_check ~default:"-" ~f:Format.status)
                     ; (if b.trusted then Widgets.badge "trusted" else trust b)
                     ])))
          ; action.view
          ]
      ; Widgets.section
          ~id:"add-payee"
          "Add a payee"
          [ N.div
              ~attrs:[ A.class_ "form-grid" ]
              [ Form.input
                  ~form
                  ~set:set_form
                  ~name:"name"
                  ~label:"Name"
                  ?error:(shown "name" name)
                  ()
              ; Form.select ~form ~set:set_form ~name:"rail" ~label:"Type" rails
              ; Form.input
                  ~form
                  ~set:set_form
                  ~name:"destination"
                  ~label:
                    (match rail with
                     | "sepa" | "sepa_instant" -> "IBAN"
                     | "lightning" -> "Lightning invoice"
                     | _ -> "Address")
                  ?error:(shown "destination" destination)
                  ()
              ; (if is_sepa
                 then
                   Form.input
                     ~form
                     ~set:set_form
                     ~name:"bic"
                     ~label:"BIC (optional)"
                     ?error:(shown "bic" bic)
                     ()
                 else N.none)
              ]
          ; Widgets.button
              ~id:"add-payee-submit"
              ~primary:true
              ~disabled:(Result.is_error request || Action.busy action)
              ~on_click:
                (match request with
                 | Error _ -> Effect.Ignore
                 | Ok r ->
                   action.run
                     ~busy:"Adding..."
                     (fun () -> page.backend.add_beneficiary ~token r)
                     ~on_ok:(fun (b : Api_types.Beneficiary.t) ->
                       let open Effect.Let_syntax in
                       let%bind () = set_form (String.Map.singleton "rail" rail) in
                       let%bind () =
                         action.set
                           (Done
                              (match b.payee_check with
                               | Some "match" ->
                                 "Added. The name matches the account holder."
                               | Some "close_match" ->
                                 "Added. The name is close to the account holder's; \
                                  check it."
                               | Some "no_match" ->
                                 "Added, but the name does not match the account holder."
                               | _ -> "Added."))
                       in
                       bump))
              "Add payee"
          ]
      ]
;;

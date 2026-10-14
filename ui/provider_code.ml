open! Core
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let view ~(page : Page.t) ~token ~bump ~(action : Action.t) ~form ~set_form challenges =
  match challenges with
  | [] -> N.none
  | _ ->
    Widgets.section
      ~id:"provider-codes"
      "Codes from the bank"
      (List.map challenges ~f:(fun (what, (c : Api_types.Sca_info.t)) ->
         let field = "code-" ^ c.challenge_id in
         let code = Check.totp (Form.get form field) in
         N.div
           ~attrs:[ A.class_ "step-open" ]
           [ N.p
               [ N.text
                   (sprintf
                      "%s: enter the code the bank sent you (%d attempts left)."
                      what
                      c.attempts_left)
               ]
           ; Form.input ~form ~set:set_form ~name:field ~label:"Code" ~placeholder:"123456" ()
           ; Widgets.button
               ~id:("provider-code-" ^ c.challenge_id)
               ~primary:true
               ~disabled:(Result.is_error code || Action.busy action)
               ~on_click:
                 (match code with
                  | Error _ -> Effect.Ignore
                  | Ok code ->
                    action.run
                      ~busy:"Sending the code..."
                      (fun () -> page.backend.provider_sca ~token c.challenge_id { code })
                      ~on_ok:(fun () ->
                        let open Effect.Let_syntax in
                        let%bind () = set_form (Map.remove form field) in
                        let%bind () = action.set (Done "Code accepted.") in
                        bump))
               "Send code"
           ]))
;;

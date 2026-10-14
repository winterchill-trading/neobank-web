open! Core
open! Async_kernel
open! Bonsai_web
open Js_of_ocaml
module Hosted = Neobank_web_model.Hosted

let host_id : (string * Dom_html.divElement Js.t) Type_equal.Id.t =
  Type_equal.Id.create ~name:"hosted-component" [%sexp_of: opaque]
;;

let host ?class_ dom_id =
  let init () =
    let div = Dom_html.createDiv Dom_html.document in
    div##.id := Js.string dom_id;
    Option.iter class_ ~f:(fun c -> div##.className := Js.string c);
    dom_id, div
  in
  Vdom.Node.widget
    ~id:host_id
    ~init
    ~update:(fun old_id el ->
      (* Same position, other element (another card): start a fresh host. *)
      if String.equal old_id dom_id then old_id, el else init ())
    ()
;;

let error_of_js what (e : Js.Unsafe.any) =
  let text =
    try Js.to_string (Js.Unsafe.meth_call e "toString" [||]) with
    | _ -> "unknown error"
  in
  Error.of_string (what ^ ": " ^ text)
;;

(* Waits on a JavaScript promise. *)
let of_promise ~what (p : Js.Unsafe.any) : Js.Unsafe.any Or_error.t Deferred.t =
  let ivar = Ivar.create () in
  let ok = Js.wrap_callback (fun v -> Ivar.fill_if_empty ivar (Ok v)) in
  let err =
    Js.wrap_callback (fun e -> Ivar.fill_if_empty ivar (Error (error_of_js what e)))
  in
  ignore
    (Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject ok; Js.Unsafe.inject err |]
     : Js.Unsafe.any);
  Ivar.read ivar
;;

let global name : Js.Unsafe.any option =
  let v : Js.Unsafe.any = Js.Unsafe.get Js.Unsafe.global (Js.string name) in
  if Js.Unsafe.equals v Js.undefined || Js.Unsafe.equals v Js.null then None else Some v
;;

let try_js what f =
  match f () with
  | x -> Ok x
  | exception Js_error.Exn e -> Error (Error.of_string (what ^ ": " ^ Js_error.message e))
  | exception exn -> Error (Error.of_exn exn)
;;

let str s = Js.Unsafe.inject (Js.string s)

module Kyc_sdk = struct
  let launch ~container ~token ~refresh ~on_message =
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Browser.load_script Hosted.kyc_sdk_script in
    match global "snsWebSdk" with
    | None -> Deferred.Or_error.error_string "the identity SDK did not load"
    | Some sdk ->
      Deferred.return
        (try_js "identity SDK" (fun () ->
           (* The token update callback must return a Promise of the new token. *)
           let refresh_cb =
             Js.wrap_callback (fun () ->
               let promise_ctor = Js.Unsafe.get Js.Unsafe.global (Js.string "Promise") in
               Js.Unsafe.new_obj
                 promise_ctor
                 [| Js.Unsafe.inject
                      (Js.wrap_callback (fun resolve reject ->
                         upon (refresh ()) (function
                           | Ok token -> Js.Unsafe.fun_call resolve [| str token |]
                           | Error e ->
                             Js.Unsafe.fun_call reject [| str (Error.to_string_hum e) |])))
                 |])
           in
           let builder =
             Js.Unsafe.meth_call sdk "init" [| str token; Js.Unsafe.inject refresh_cb |]
           in
           let builder =
             Js.Unsafe.meth_call
               builder
               "withConf"
               [| Js.Unsafe.inject (Js.Unsafe.obj [| "lang", str "en" |]) |]
           in
           let builder =
             Js.Unsafe.meth_call
               builder
               "withOptions"
               [| Js.Unsafe.inject
                    (Js.Unsafe.obj
                       [| "addViewportTag", Js.Unsafe.inject Js._false
                        ; "adaptIframeHeight", Js.Unsafe.inject Js._true
                       |])
               |]
           in
           let builder =
             List.fold
               [ "idCheck.onStepCompleted"
               ; "idCheck.onApplicantSubmitted"
               ; "idCheck.onApplicantStatusChanged"
               ; "idCheck.onError"
               ]
               ~init:builder
               ~f:(fun b event ->
                 Js.Unsafe.meth_call
                   b
                   "on"
                   [| str event
                    ; Js.Unsafe.inject
                        (Js.wrap_callback (fun (_ : Js.Unsafe.any) -> on_message event))
                   |])
           in
           let instance = Js.Unsafe.meth_call builder "build" [||] in
           ignore
             (Js.Unsafe.meth_call instance "launch" [| str ("#" ^ container) |]
              : Js.Unsafe.any)))
  ;;
end

module Card_plugin = struct
  let plugin () =
    match global "StrigaUXPlugin" with
    | Some p -> Ok p
    | None -> Or_error.error_string "the card display plugin did not load"
  ;;

  let create (d : Hosted.Card_display.t) =
    let open Deferred.Or_error.Let_syntax in
    let%bind () = Browser.load_script (Hosted.Card_display.script d) in
    Deferred.return
      (let%bind.Or_error p = plugin () in
       try_js "card display" (fun () ->
         ignore
           (Js.Unsafe.meth_call
              p
              "create"
              [| str d.ui_secret
               ; Js.Unsafe.inject
                   (Js.Unsafe.obj [| "applicationId", str d.application_id |])
              |]
            : Js.Unsafe.any)))
  ;;

  let request_consent ~user_id =
    match plugin () with
    | Error e -> Deferred.return (Error e)
    | Ok p ->
      (match
         try_js "card display" (fun () ->
           Js.Unsafe.meth_call
             p
             "requestConsent"
             [| Js.Unsafe.inject (Js.Unsafe.obj [| "userId", str user_id |]) |])
       with
       | Error e -> Deferred.return (Error e)
       | Ok promise ->
         let%map.Deferred.Or_error response = of_promise ~what:"consent" promise in
         Js.to_string (Js.Unsafe.get response (Js.string "challengeId")))
  ;;

  let render ~key ~dom_id ~card_id ~auth_token ~masked =
    match plugin () with
    | Error e -> Deferred.return (Error e)
    | Ok p ->
      let ivar = Ivar.create () in
      let options =
        Js.Unsafe.obj
          (Array.of_list
             (List.filter_opt
                [ Some ("id", str dom_id)
                ; Some ("cardId", str card_id)
                ; Some ("authToken", str auth_token)
                ; Option.map masked ~f:(fun m -> "maskedCardNumber", str m)
                ; Some ("enableCardNumberCopy", Js.Unsafe.inject Js._true)
                ]))
      in
      let callback =
        Js.wrap_callback (fun (response : Js.Unsafe.any) ->
          (* The plugin answers [success] or [error], as a string or a field. *)
          let text =
            try Js.to_string (Json.output response) with
            | _ -> ""
          in
          if String.is_substring text ~substring:"error"
          then Ivar.fill_if_empty ivar (Or_error.error_s [%message "card display" text])
          else Ivar.fill_if_empty ivar (Ok ()))
      in
      (match
         try_js "card display" (fun () ->
           ignore
             (Js.Unsafe.meth_call
                p
                "render"
                [| str key; Js.Unsafe.inject options; Js.Unsafe.inject callback |]
              : Js.Unsafe.any))
       with
       | Error e -> Ivar.fill_if_empty ivar (Error e)
       | Ok () -> ());
      Ivar.read ivar
  ;;
end

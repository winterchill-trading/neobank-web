open! Core
open! Async_kernel
open Neobank_api_types
open Neobank_web_model
module A = Api_types

type 'a res = ('a, Api_failure.t) Result.t Deferred.t

type t =
  { description : string
  ; is_mock : bool
  ; signup : A.Auth.Signup_request.t -> A.Auth.Signup_response.t res
  ; login : A.Auth.Login_request.t -> A.Auth.Session.t res
  ; step_up : token:string -> A.Auth.Step_up_request.t -> A.Auth.Session.t res
  ; logout : token:string -> unit res
  ; me : token:string -> A.Me.t res
  ; kyc_status : token:string -> A.Kyc.Status.t res
  ; kyc_start : token:string -> A.Kyc.Start_request.t -> A.Kyc.Session.t res
  ; kyc_complete : token:string -> A.Kyc.Complete_request.t -> A.Kyc.Status.t res
  ; accounts : token:string -> A.Account.t list res
  ; account : token:string -> string -> A.Account.t res
  ; statement :
      token:string -> ?from:string -> ?until:string -> string -> A.Statement.t res
  ; deposit : token:string -> A.Sandbox.Deposit.t -> unit res
  ; kyc_decision : token:string -> A.Sandbox.Kyc_decision.t -> A.Kyc.Status.t res
  ; beneficiaries : token:string -> A.Beneficiary.t list res
  ; add_beneficiary : token:string -> A.Beneficiary.Request.t -> A.Beneficiary.t res
  ; trust_beneficiary : token:string -> string -> A.Beneficiary.t res
  ; fee_quote : token:string -> A.Fee_quote.Request.t -> A.Fee_quote.t res
  ; create_transfer : token:string -> A.Transfer.Request.t -> A.Transfer.t res
  ; transfers : token:string -> A.Transfer.t list res
  ; transfer : token:string -> string -> A.Transfer.t res
  ; confirm_sca :
      token:string -> string -> A.Transfer.Confirm_request.t -> A.Transfer.t res
  ; restart_sca : token:string -> string -> A.Transfer.t res
  ; cancel_transfer : token:string -> string -> A.Transfer.t res
  ; swap_quote : token:string -> A.Swap.Quote_request.t -> A.Swap.t res
  ; execute_swap : token:string -> string -> A.Swap.t res
  ; swaps : token:string -> A.Swap.t list res
  ; cards : token:string -> A.Card.t list res
  ; issue_card : token:string -> A.Card.Issue_request.t -> A.Card.t res
  ; freeze_card : token:string -> string -> A.Card.t res
  ; unfreeze_card : token:string -> string -> A.Card.t res
  ; set_card_limits : token:string -> string -> A.Card.Limits.t -> A.Card.t res
  ; card_display : token:string -> string -> Hosted.Card_display.t res
  ; card_consent : token:string -> string -> challenge_id:string -> A.Sca_info.t res
  ; pan_token :
      token:string
      -> string
      -> [ `Token of A.Card.Pan_token.t | `Challenge of A.Sca_info.t ] res
  ; standing_orders : token:string -> A.Standing_order.t list res
  ; create_standing_order :
      token:string -> A.Standing_order.Request.t -> A.Standing_order.t res
  ; cancel_standing_order : token:string -> string -> A.Standing_order.t res
  ; provider_sca : token:string -> string -> A.Sca_confirm_request.t -> unit res
  ; reviews : ops_token:string -> A.Review.t list res
  ; decide_review :
      ops_token:string -> string -> A.Review.Decide_request.t -> A.Review.t res
  ; simulated_pan : string -> string option
  ; subscribe : token:string -> on_push:(A.Ws_push.t -> unit) -> unit -> unit
  }

(* {2 Live} *)

(* A plain XMLHttpRequest: Async_js.Http only speaks GET and POST. *)
let request ~meth ~headers ~url ~body =
  let open Js_of_ocaml in
  let result = Ivar.create () in
  let req = XmlHttpRequest.create () in
  req##_open (Js.string meth) (Js.string url) Js._true;
  List.iter headers ~f:(fun (k, v) -> req##setRequestHeader (Js.string k) (Js.string v));
  req##.onreadystatechange
  := Js.wrap_callback (fun () ->
       match req##.readyState with
       | DONE ->
         let content =
           Js.Opt.case req##.responseText (fun () -> "") (fun s -> Js.to_string s)
         in
         (* Status 0 is a request that never got an answer (CORS, offline). *)
         if req##.status = 0
         then
           Ivar.fill_if_empty result (Error (Api_failure.network ("cannot reach " ^ url)))
         else Ivar.fill_if_empty result (Ok (req##.status, content))
       | _ -> ());
  (match body with
   | None -> req##send Js.null
   | Some body -> req##send (Js.some (Js.string body)));
  Ivar.read result
;;

let decode of_yojson body =
  match Or_error.try_with (fun () -> of_yojson (Yojson.Safe.from_string body)) with
  | Ok x -> Ok x
  | Error e -> Error (Api_failure.decode e)
;;

let raw ~base_url ?token ?body ~meth path =
  let idempotent =
    String.equal meth "POST" && not (String.is_prefix path ~prefix:"/auth/")
  in
  let headers =
    List.filter_opt
      [ Option.map token ~f:(fun token -> "Authorization", "Bearer " ^ token)
      ; Option.map body ~f:(fun _ -> "Content-Type", "application/json")
      ; Option.some_if idempotent ("Idempotency-Key", Browser.random_id ())
      ]
  in
  request
    ~meth
    ~headers
    ~url:(base_url ^ path)
    ~body:(Option.map body ~f:Yojson.Safe.to_string)
;;

let call ~base_url ?token ?body ?(meth = "GET") path of_yojson =
  match%map raw ~base_url ?token ?body ~meth path with
  | Error e -> Error e
  | Ok (code, content) when code >= 200 && code < 300 -> decode of_yojson content
  | Ok (status, content) -> Error (Api_failure.of_response ~status content)
;;

let ignore_body (_ : Yojson.Safe.t) = ()
let list of_yojson json = Yojson.Safe.Util.to_list json |> List.map ~f:of_yojson

let ws_url ~base_url ~token =
  let path = Endpoint.ws ~token in
  let base =
    if String.is_prefix base_url ~prefix:"http"
    then base_url
    else
      let open Js_of_ocaml in
      Js.to_string Dom_html.window##.location##.origin ^ base_url
  in
  match String.chop_prefix base ~prefix:"https" with
  | Some rest -> "wss" ^ rest ^ path
  | None ->
    (match String.chop_prefix base ~prefix:"http" with
     | Some rest -> "ws" ^ rest ^ path
     | None -> base ^ path)
;;

let subscribe_live ~base_url ~token ~on_push =
  let open Js_of_ocaml in
  let closed = ref false in
  let socket = ref None in
  let rec connect () =
    if not !closed
    then (
      let ws = new%js WebSockets.webSocket (Js.string (ws_url ~base_url ~token)) in
      socket := Some ws;
      ws##.onmessage
      := Dom.handler (fun ev ->
           (match
              Or_error.bind
                (Or_error.try_with (fun () ->
                   Yojson.Safe.from_string (Js.to_string ev##.data)))
                ~f:A.Ws_push.of_yojson
            with
            | Ok push -> on_push push
            | Error _ -> ());
           Js._false);
      ws##.onclose
      := Dom.handler (fun _ ->
           (* Sessions expire and proxies drop idle sockets; try again in 5 s, which is
              also the slowest the pages refresh on their own. *)
           ignore
             (Dom_html.window##setTimeout
                (Js.wrap_callback (fun () -> connect ()))
                (Js.number_of_float 5000.)
              : Dom_html.timeout_id);
           Js._false))
  in
  connect ();
  fun () ->
    closed := true;
    Option.iter !socket ~f:(fun ws -> ws##close)
;;

let live ~base_url =
  let call ?token ?body ?meth path f = call ~base_url ?token ?body ?meth path f in
  let post ?token path body f = call ?token ~meth:"POST" ~body path f in
  let empty = `Assoc [] in
  { description = base_url
  ; is_mock = false
  ; signup =
      (fun r ->
        post
          Endpoint.signup
          (A.Auth.Signup_request.yojson_of_t r)
          A.Auth.Signup_response.t_of_yojson)
  ; login =
      (fun r ->
        post
          Endpoint.login
          (A.Auth.Login_request.yojson_of_t r)
          A.Auth.Session.t_of_yojson)
  ; step_up =
      (fun ~token r ->
        post
          ~token
          Endpoint.step_up
          (A.Auth.Step_up_request.yojson_of_t r)
          A.Auth.Session.t_of_yojson)
  ; logout =
      (fun ~token ->
        match%map raw ~base_url ~token ~body:empty ~meth:"POST" Endpoint.logout with
        | Ok (code, _) when code >= 200 && code < 300 -> Ok ()
        | Ok (status, content) -> Error (Api_failure.of_response ~status content)
        | Error e -> Error e)
  ; me = (fun ~token -> call ~token Endpoint.me A.Me.t_of_yojson)
  ; kyc_status = (fun ~token -> call ~token Endpoint.kyc A.Kyc.Status.t_of_yojson)
  ; kyc_start =
      (fun ~token r ->
        post
          ~token
          Endpoint.kyc_start
          (A.Kyc.Start_request.yojson_of_t r)
          A.Kyc.Session.t_of_yojson)
  ; kyc_complete =
      (fun ~token r ->
        post
          ~token
          Endpoint.kyc_complete
          (A.Kyc.Complete_request.yojson_of_t r)
          A.Kyc.Status.t_of_yojson)
  ; accounts = (fun ~token -> call ~token Endpoint.accounts (list A.Account.t_of_yojson))
  ; account = (fun ~token id -> call ~token (Endpoint.account id) A.Account.t_of_yojson)
  ; statement =
      (fun ~token ?from ?until id ->
        call ~token (Endpoint.statement ?from ?until id) A.Statement.t_of_yojson)
  ; deposit =
      (fun ~token r ->
        post ~token Endpoint.sandbox_deposit (A.Sandbox.Deposit.yojson_of_t r) ignore_body)
  ; kyc_decision =
      (fun ~token r ->
        post
          ~token
          Endpoint.sandbox_kyc_decision
          (A.Sandbox.Kyc_decision.yojson_of_t r)
          A.Kyc.Status.t_of_yojson)
  ; beneficiaries =
      (fun ~token -> call ~token Endpoint.beneficiaries (list A.Beneficiary.t_of_yojson))
  ; add_beneficiary =
      (fun ~token r ->
        post
          ~token
          Endpoint.beneficiaries
          (A.Beneficiary.Request.yojson_of_t r)
          A.Beneficiary.t_of_yojson)
  ; trust_beneficiary =
      (fun ~token id ->
        post ~token (Endpoint.trust_beneficiary id) empty A.Beneficiary.t_of_yojson)
  ; fee_quote =
      (fun ~token r ->
        post
          ~token
          Endpoint.fee_quote
          (A.Fee_quote.Request.yojson_of_t r)
          A.Fee_quote.t_of_yojson)
  ; create_transfer =
      (fun ~token r ->
        post
          ~token
          Endpoint.transfers
          (A.Transfer.Request.yojson_of_t r)
          A.Transfer.t_of_yojson)
  ; transfers =
      (fun ~token -> call ~token Endpoint.transfers (list A.Transfer.t_of_yojson))
  ; transfer =
      (fun ~token id -> call ~token (Endpoint.transfer id) A.Transfer.t_of_yojson)
  ; confirm_sca =
      (fun ~token id r ->
        post
          ~token
          (Endpoint.confirm_sca id)
          (A.Transfer.Confirm_request.yojson_of_t r)
          A.Transfer.t_of_yojson)
  ; restart_sca =
      (fun ~token id ->
        post ~token (Endpoint.restart_sca id) empty A.Transfer.t_of_yojson)
  ; cancel_transfer =
      (fun ~token id ->
        post ~token (Endpoint.cancel_transfer id) empty A.Transfer.t_of_yojson)
  ; swap_quote =
      (fun ~token r ->
        post
          ~token
          Endpoint.swap_quote
          (A.Swap.Quote_request.yojson_of_t r)
          A.Swap.t_of_yojson)
  ; execute_swap =
      (fun ~token id -> post ~token (Endpoint.execute_swap id) empty A.Swap.t_of_yojson)
  ; swaps = (fun ~token -> call ~token Endpoint.swaps (list A.Swap.t_of_yojson))
  ; cards = (fun ~token -> call ~token Endpoint.cards (list A.Card.t_of_yojson))
  ; issue_card =
      (fun ~token r ->
        post ~token Endpoint.cards (A.Card.Issue_request.yojson_of_t r) A.Card.t_of_yojson)
  ; freeze_card =
      (fun ~token id -> post ~token (Endpoint.freeze_card id) empty A.Card.t_of_yojson)
  ; unfreeze_card =
      (fun ~token id -> post ~token (Endpoint.unfreeze_card id) empty A.Card.t_of_yojson)
  ; set_card_limits =
      (fun ~token id l ->
        post
          ~token
          (Endpoint.card_limits id)
          (A.Card.Limits.yojson_of_t l)
          A.Card.t_of_yojson)
  ; card_display =
      (fun ~token id ->
        call ~token (Endpoint.card_display id) Hosted.Card_display.t_of_yojson)
  ; card_consent =
      (fun ~token id ~challenge_id ->
        post
          ~token
          (Endpoint.card_consent id)
          (`Assoc [ "challenge_id", `String challenge_id ])
          A.Sca_info.t_of_yojson)
  ; pan_token =
      (fun ~token id ->
        (* 200 is the token; 202 a Sca_required body naming the provider's challenge. *)
        match%map
          raw ~base_url ~token ~body:empty ~meth:"POST" (Endpoint.pan_token id)
        with
        | Error e -> Error e
        | Ok (202, content) ->
          Result.map (decode A.Sca_required.t_of_yojson content) ~f:(fun r ->
            `Challenge r.challenge)
        | Ok (code, content) when code >= 200 && code < 300 ->
          Result.map (decode A.Card.Pan_token.t_of_yojson content) ~f:(fun t -> `Token t)
        | Ok (status, content) -> Error (Api_failure.of_response ~status content))
  ; standing_orders =
      (fun ~token ->
        call ~token Endpoint.standing_orders (list A.Standing_order.t_of_yojson))
  ; create_standing_order =
      (fun ~token r ->
        post
          ~token
          Endpoint.standing_orders
          (A.Standing_order.Request.yojson_of_t r)
          A.Standing_order.t_of_yojson)
  ; cancel_standing_order =
      (fun ~token id ->
        call
          ~token
          ~meth:"DELETE"
          (Endpoint.standing_order id)
          A.Standing_order.t_of_yojson)
  ; provider_sca =
      (fun ~token id r ->
        post
          ~token
          (Endpoint.provider_sca id)
          (A.Sca_confirm_request.yojson_of_t r)
          ignore_body)
  ; reviews =
      (fun ~ops_token ->
        call ~token:ops_token Endpoint.reviews (list A.Review.t_of_yojson))
  ; decide_review =
      (fun ~ops_token id r ->
        post
          ~token:ops_token
          (Endpoint.review_decision id)
          (A.Review.Decide_request.yojson_of_t r)
          A.Review.t_of_yojson)
  ; simulated_pan = (fun _ -> None)
  ; subscribe = (fun ~token ~on_push -> subscribe_live ~base_url ~token ~on_push)
  }
;;

(* {2 Mock} *)

let mock () =
  let bank = Mock_bank.create ~now:(Browser.now_ms ()) in
  let now () = Browser.now_ms () in
  let r x = return x in
  { description = "mock data"
  ; is_mock = true
  ; signup = (fun x -> r (Mock_bank.signup bank ~now:(now ()) x))
  ; login = (fun x -> r (Mock_bank.login bank ~now:(now ()) x))
  ; step_up = (fun ~token x -> r (Mock_bank.step_up bank ~now:(now ()) ~token x))
  ; logout = (fun ~token -> r (Mock_bank.logout bank ~token))
  ; me = (fun ~token -> r (Mock_bank.me bank ~now:(now ()) ~token))
  ; kyc_status = (fun ~token -> r (Mock_bank.kyc_status bank ~token))
  ; kyc_start = (fun ~token x -> r (Mock_bank.kyc_start bank ~now:(now ()) ~token x))
  ; kyc_complete =
      (fun ~token x -> r (Mock_bank.kyc_complete bank ~now:(now ()) ~token x))
  ; accounts = (fun ~token -> r (Mock_bank.accounts bank ~token))
  ; account = (fun ~token id -> r (Mock_bank.account bank ~token id))
  ; statement =
      (fun ~token ?from ?until id ->
        r (Mock_bank.statement bank ~now:(now ()) ~token ?from ?until id))
  ; deposit =
      (fun ~token x ->
        r (Result.map (Mock_bank.deposit bank ~now:(now ()) ~token x) ~f:ignore))
  ; kyc_decision =
      (fun ~token x -> r (Mock_bank.kyc_decision bank ~now:(now ()) ~token x))
  ; beneficiaries = (fun ~token -> r (Mock_bank.beneficiaries bank ~token))
  ; add_beneficiary =
      (fun ~token x -> r (Mock_bank.add_beneficiary bank ~now:(now ()) ~token x))
  ; trust_beneficiary =
      (fun ~token id -> r (Mock_bank.trust_beneficiary bank ~now:(now ()) ~token id))
  ; fee_quote = (fun ~token x -> r (Mock_bank.fee_quote bank ~token x))
  ; create_transfer =
      (fun ~token x -> r (Mock_bank.create_transfer bank ~now:(now ()) ~token x))
  ; transfers = (fun ~token -> r (Mock_bank.transfers bank ~token))
  ; transfer = (fun ~token id -> r (Mock_bank.transfer bank ~token id))
  ; confirm_sca =
      (fun ~token id x -> r (Mock_bank.confirm_sca bank ~now:(now ()) ~token id x))
  ; restart_sca =
      (fun ~token id -> r (Mock_bank.restart_sca bank ~now:(now ()) ~token id))
  ; cancel_transfer =
      (fun ~token id -> r (Mock_bank.cancel_transfer bank ~now:(now ()) ~token id))
  ; swap_quote = (fun ~token x -> r (Mock_bank.swap_quote bank ~now:(now ()) ~token x))
  ; execute_swap =
      (fun ~token id -> r (Mock_bank.execute_swap bank ~now:(now ()) ~token id))
  ; swaps = (fun ~token -> r (Mock_bank.swaps bank ~token))
  ; cards = (fun ~token -> r (Mock_bank.cards bank ~token))
  ; issue_card = (fun ~token x -> r (Mock_bank.issue_card bank ~now:(now ()) ~token x))
  ; freeze_card = (fun ~token id -> r (Mock_bank.freeze_card bank ~token id))
  ; unfreeze_card = (fun ~token id -> r (Mock_bank.unfreeze_card bank ~token id))
  ; set_card_limits =
      (fun ~token id l -> r (Mock_bank.set_card_limits bank ~now:(now ()) ~token id l))
  ; card_display = (fun ~token id -> r (Mock_bank.card_display bank ~token id))
  ; card_consent =
      (fun ~token id ~challenge_id ->
        r (Mock_bank.card_consent bank ~now:(now ()) ~token id ~challenge_id))
  ; pan_token = (fun ~token id -> r (Mock_bank.pan_token bank ~now:(now ()) ~token id))
  ; standing_orders = (fun ~token -> r (Mock_bank.standing_orders bank ~token))
  ; create_standing_order =
      (fun ~token x -> r (Mock_bank.create_standing_order bank ~now:(now ()) ~token x))
  ; cancel_standing_order =
      (fun ~token id -> r (Mock_bank.cancel_standing_order bank ~token id))
  ; provider_sca =
      (fun ~token id x -> r (Mock_bank.provider_sca bank ~now:(now ()) ~token id x))
  ; reviews = (fun ~ops_token:_ -> r (Mock_bank.reviews bank))
  ; decide_review =
      (fun ~ops_token:_ id x -> r (Mock_bank.decide_review bank ~now:(now ()) id x))
  ; simulated_pan = Mock_bank.simulated_pan bank
  ; subscribe = (fun ~token:_ ~on_push:_ () -> ())
  }
;;

let of_query query =
  let find k = List.Assoc.find query ~equal:String.equal k in
  match find "mock", find "api" with
  | Some ("1" | "true" | ""), _ -> mock ()
  | _, Some base_url when not (String.is_empty base_url) ->
    live ~base_url:(String.chop_suffix_if_exists base_url ~suffix:"/")
  | _ -> live ~base_url:"/api"
;;

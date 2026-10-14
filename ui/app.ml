open! Core
open! Bonsai_web
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let header ~(backend : Backend.t) ~route ~session ~logout =
  let current = Route.section route in
  N.header
    ~attrs:[ A.class_ "top" ]
    [ N.a ~attrs:[ A.href "#/dashboard"; A.class_ "brand" ] [ N.text "Neobank demo" ]
    ; (match (session : Session.t option) with
       | None -> N.nav []
       | Some _ ->
         N.nav
           (List.map Route.menu ~f:(fun r ->
              Widgets.link
                ~attrs:(if Route.equal r current then [ A.class_ "active" ] else [])
                r
                [ N.text (Route.title r) ])))
    ; N.span
        ~attrs:
          [ A.classes [ "badge"; (if backend.is_mock then "mock" else "live") ]
          ; A.id "backend"
          ]
        [ N.text backend.description ]
    ; (match session with
       | None ->
         N.div
           ~attrs:[ A.class_ "account" ]
           [ Widgets.link ~attrs:[ A.class_ "btn" ] Sign_in [ N.text "Sign in" ]
           ; Widgets.link
               ~attrs:[ A.classes [ "btn"; "primary" ] ]
               Sign_up
               [ N.text "Open an account" ]
           ]
       | Some _ ->
         N.div
           ~attrs:[ A.class_ "account" ]
           [ Widgets.link Review [ N.text "Operators" ]
           ; Widgets.button ~id:"sign-out" ~on_click:logout "Sign out"
           ])
    ]
;;

let component ~(backend : Backend.t) ~route (local_ graph) =
  let open Bonsai.Let_syntax in
  let restored = if backend.is_mock then None else Session.restore () in
  let session, set_session =
    Bonsai.state restored ~equal:[%equal: Session.t option] graph
  in
  let refresh, set_refresh = Bonsai.state 0 graph in
  let bump =
    let%map refresh and set_refresh in
    set_refresh (refresh + 1)
  in
  (* Pushes from the API (payment, account, KYC, card updates) reload what is showing. *)
  let closer = ref (fun () -> ()) in
  let push_callback =
    let%map bump in
    fun (s : Session.t option) ->
      Effect.of_thunk (fun () ->
        !closer ();
        closer
        := fun () ->
             ();
             match s with
             | None -> ()
             | Some s ->
               closer
               := backend.subscribe ~token:s.token ~on_push:(fun _ ->
                    Effect.Expert.handle_non_dom_event_exn bump))
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: Session.t option]
    session
    ~callback:push_callback
    graph;
  let page : Page.t = { backend; session; refresh; bump; route } in
  let login =
    let%map set_session in
    fun s ->
      if not backend.is_mock then Session.save (Some s);
      set_session (Some s)
  in
  let sign_in = Page_sign_in.component page ~login graph in
  let sign_up = Page_sign_up.component page graph in
  let onboarding = Page_onboarding.component page graph in
  let dashboard = Page_dashboard.component page graph in
  let accounts = Page_accounts.list_component page graph in
  let account = Page_accounts.detail_component page graph in
  let send = Page_send.component page graph in
  let transfer = Page_transfer.component page graph in
  let swap = Page_swap.component page graph in
  let cards = Page_cards.component page graph in
  let standing_orders = Page_standing_orders.component page graph in
  let statements = Page_statements.component page graph in
  let beneficiaries = Page_beneficiaries.component page graph in
  let review = Page_review.component page graph in
  let%map route
  and session
  and set_session
  and sign_in
  and sign_up
  and onboarding
  and dashboard
  and accounts
  and account
  and send
  and transfer
  and swap
  and cards
  and standing_orders
  and statements
  and beneficiaries
  and review in
  let logout =
    let open Effect.Let_syntax in
    let%bind () =
      match session with
      | None -> return ()
      | Some s ->
        let%map (_ : (unit, Api_failure.t) Result.t) =
          Effect.of_deferred_thunk (fun () -> backend.logout ~token:s.token)
        in
        ()
    in
    let%bind () = Effect.of_thunk (fun () -> Session.save None) in
    let%bind () = set_session None in
    Nav.navigate Sign_in
  in
  let page =
    match (route : Route.t) with
    | Sign_in -> sign_in
    | Sign_up -> sign_up
    | Onboarding -> onboarding
    | Dashboard -> dashboard
    | Accounts -> accounts
    | Account _ -> account
    | Send -> send
    | Transfer _ -> transfer
    | Swap -> swap
    | Cards -> cards
    | Standing_orders -> standing_orders
    | Statements -> statements
    | Beneficiaries -> beneficiaries
    | Review -> review
  in
  N.div
    ~attrs:[ A.class_ "app" ]
    [ header ~backend ~route ~session ~logout; N.main ~attrs:[ A.id "page" ] [ page ] ]
;;

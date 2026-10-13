open! Core
open Neobank_web_model

let all : Route.t list =
  [ Sign_in
  ; Sign_up
  ; Onboarding
  ; Dashboard
  ; Accounts
  ; Account "acc_1"
  ; Send
  ; Transfer "tr_9"
  ; Swap
  ; Cards
  ; Standing_orders
  ; Statements
  ; Beneficiaries
  ; Review
  ]
;;

let%expect_test "every route round-trips through its fragment" =
  List.iter all ~f:(fun r ->
    let hash = Route.to_hash r in
    [%test_result: Route.t] ~expect:r (Route.of_hash hash);
    printf "%-22s %s\n" hash (Route.title r));
  [%expect
    {|
    #/sign-in              Sign in
    #/sign-up              Open an account
    #/onboarding           Verify your identity
    #/dashboard            Home
    #/accounts             Accounts
    #/accounts/acc_1       Account
    #/send                 Send
    #/transfers/tr_9       Payment
    #/swap                 Swap
    #/cards                Cards
    #/standing-orders      Standing orders
    #/statements           Statements
    #/beneficiaries        Payees
    #/review               Review queue
    |}]
;;

let%expect_test "unknown fragments land on the dashboard" =
  List.iter [ ""; "#"; "#/nope"; "/accounts/a/b" ] ~f:(fun h ->
    print_s [%sexp (Route.of_hash h : Route.t)]);
  [%expect
    {|
    Dashboard
    Dashboard
    Dashboard
    Dashboard
    |}]
;;

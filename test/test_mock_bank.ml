open! Core
open Neobank_api_types
open Neobank_web_model
module A = Api_types

let now = ref 1_760_430_000_000.
let tick ms = now := !now +. ms

let ok = function
  | Ok x -> x
  | Error e -> raise_s [%message "unexpected refusal" (e : Api_failure.t)]
;;

let show_error = function
  | Ok _ -> print_endline "ok"
  | Error (e : Api_failure.t) -> printf "%d %s: %s\n" e.status e.error e.message
;;

let balances bank ~token =
  Mock_bank.accounts bank ~token
  |> ok
  |> List.iter ~f:(fun (a : A.Account.t) ->
    let b = Option.value_exn a.balance in
    printf
      "%-5s available %s held %s\n"
      a.currency
      (Format.amount b.available)
      (Format.amount b.held))
;;

let login bank =
  (Mock_bank.login
     bank
     ~now:!now
     { email = Mock_bank.demo_email; password = Mock_bank.demo_password; totp = "123456" }
   |> ok)
    .token
;;

let%expect_test "seeded customer" =
  let bank = Mock_bank.create ~now:!now in
  show_error
    (Mock_bank.login
       bank
       ~now:!now
       { email = Mock_bank.demo_email
       ; password = Mock_bank.demo_password
       ; totp = "000000"
       });
  let token = login bank in
  balances bank ~token;
  let eur = Mock_bank.account bank ~token "acc_eur_cus_demo" |> ok in
  print_s [%sexp (eur.iban : string option), (eur.bic : string option)];
  print_s [%sexp (Check.iban (Option.value_exn eur.iban) |> Result.is_ok : bool)];
  let st = Mock_bank.statement bank ~now:!now ~token "acc_eur_cus_demo" |> ok in
  print_string
    (Statement_csv.to_string st |> String.substr_replace_all ~pattern:"\r" ~with_:"");
  [%expect
    {|
    401 invalid_credentials: the email, password or code is wrong
    EUR   available €2,189.00 held €18.90
    BTC   available 0.0425 BTC held 0 BTC
    ETH   available 1.2 ETH held 0 ETH
    USDC  available 500 USDC held 0 USDC
    USDT  available 0 USDT held 0 USDT
    ((DE62370400440532013001) (DEMODEFFXXX))
    true
    at,amount,currency,code,reference,balance
    2025-09-19T08:20:00.000Z,3200.00,EUR,sepa_in,Salary September,3200.00
    2025-09-20T08:20:00.000Z,-950.00,EUR,sepa_out,Rent October,2250.00
    2025-10-12T08:20:00.000Z,-42.10,EUR,sepa_out,Groceries,2207.90
    |}]
;;

let%expect_test "a SEPA payment: quote, our SCA, settled" =
  let bank = Mock_bank.create ~now:!now in
  let token = login bank in
  let q =
    Mock_bank.fee_quote
      bank
      ~token
      { account_id = "acc_eur_cus_demo"
      ; rail = "sepa_instant"
      ; amount = { amount = "100"; currency = "EUR" }
      }
    |> ok
  in
  print_s [%sexp (q : A.Fee_quote.t)];
  let tr =
    Mock_bank.create_transfer
      bank
      ~now:!now
      ~token
      { account_id = "acc_eur_cus_demo"
      ; beneficiary_id = "ben_landlord"
      ; amount = { amount = "100"; currency = "EUR" }
      ; rail = Some "sepa_instant"
      ; reference = Some "Deposit"
      ; wallet_provider = None
      }
    |> ok
  in
  print_s [%sexp (tr.status : string), (tr.sca : A.Sca_info.t option)];
  show_error
    (Mock_bank.confirm_sca
       bank
       ~now:!now
       ~token
       tr.id
       { code = "123456"; password = Some "wrong" });
  let tr = Mock_bank.transfer bank ~token tr.id |> ok in
  print_s [%sexp (Option.map tr.sca ~f:(fun s -> s.attempts_left) : int option)];
  let tr =
    Mock_bank.confirm_sca
      bank
      ~now:!now
      ~token
      tr.id
      { code = "123456"; password = Some Mock_bank.demo_password }
    |> ok
  in
  print_s [%sexp (tr.status : string)];
  balances bank ~token;
  [%expect
    {|
    ((rail sepa_instant) (amount ((amount 100.00) (currency EUR)))
     (fee ((amount 0.25) (currency EUR)))
     (total ((amount 100.25) (currency EUR))))
    (awaiting_sca
     (((challenge_id sca_9) (factors (knowledge possession))
       (expires_at 2025-10-14T08:25:00.000Z) (attempts_left 5))))
    422 wrong_code: the code is wrong
    (4)
    settled
    EUR   available €2,088.75 held €18.90
    BTC   available 0.0425 BTC held 0 BTC
    ETH   available 1.2 ETH held 0 ETH
    USDC  available 500 USDC held 0 USDC
    USDT  available 0 USDT held 0 USDT
    |}]
;;

let%expect_test "on-chain: our SCA, then the provider's code" =
  let bank = Mock_bank.create ~now:!now in
  let token = login bank in
  let tr =
    Mock_bank.create_transfer
      bank
      ~now:!now
      ~token
      { account_id = "acc_btc_cus_demo"
      ; beneficiary_id = "ben_cold"
      ; amount = { amount = "0.01"; currency = "BTC" }
      ; rail = None
      ; reference = None
      ; wallet_provider = None
      }
    |> ok
  in
  let tr =
    Mock_bank.confirm_sca
      bank
      ~now:!now
      ~token
      tr.id
      { code = "111111"; password = Some Mock_bank.demo_password }
    |> ok
  in
  print_s [%sexp (tr.status : string), (Option.is_some tr.provider_sca : bool)];
  let tr =
    Mock_bank.confirm_sca bank ~now:!now ~token tr.id { code = "222222"; password = None }
    |> ok
  in
  print_s [%sexp (tr.status : string)];
  [%expect {|
    (submitted true)
    settled
    |}]
;;

let%expect_test "large payments wait for an operator" =
  let bank = Mock_bank.create ~now:!now in
  let token = login bank in
  let tr =
    Mock_bank.create_transfer
      bank
      ~now:!now
      ~token
      { account_id = "acc_eur_cus_demo"
      ; beneficiary_id = "ben_landlord"
      ; amount = { amount = "1500"; currency = "EUR" }
      ; rail = None
      ; reference = None
      ; wallet_provider = None
      }
    |> ok
  in
  print_s [%sexp (tr.status : string), (tr.message : string option)];
  let review = List.hd_exn (Mock_bank.reviews bank |> ok) in
  print_s [%sexp (review : A.Review.t)];
  let _ =
    Mock_bank.decide_review
      bank
      ~now:!now
      review.id
      { verdict = "approve"; operator = "op"; note = None }
    |> ok
  in
  let tr = Mock_bank.transfer bank ~token tr.id |> ok in
  print_s [%sexp (tr.status : string), (Option.is_some tr.sca : bool)];
  show_error
    (Mock_bank.create_transfer
       bank
       ~now:!now
       ~token
       { account_id = "acc_eur_cus_demo"
       ; beneficiary_id = "ben_landlord"
       ; amount = { amount = "99999"; currency = "EUR" }
       ; rail = None
       ; reference = None
       ; wallet_provider = None
       });
  [%expect
    {|
    (under_review ("under review"))
    ((id rev_9) (reference tr_8) (customer_id cus_demo) (severity review)
     (reasons ("large_amount: above the 1,000 EUR review threshold"))
     (submitted_at 2025-10-14T08:20:00.000Z) (status pending) (operator ()))
    (awaiting_sca true)
    422 insufficient_funds: the amount and fee are more than the available balance
    |}]
;;

let%expect_test "swaps need a fresh SCA" =
  let bank = Mock_bank.create ~now:!now in
  let token = login bank in
  let s =
    Mock_bank.swap_quote
      bank
      ~now:!now
      ~token
      { from_account_id = "acc_btc_cus_demo"
      ; to_account_id = "acc_eur_cus_demo"
      ; sell = { amount = "0.01"; currency = "BTC" }
      }
    |> ok
  in
  print_s [%sexp (s : A.Swap.t)];
  tick (6. *. Clock.minute_ms);
  show_error (Mock_bank.execute_swap bank ~now:!now ~token s.id);
  let s =
    Mock_bank.swap_quote
      bank
      ~now:!now
      ~token
      { from_account_id = "acc_btc_cus_demo"
      ; to_account_id = "acc_eur_cus_demo"
      ; sell = { amount = "0.01"; currency = "BTC" }
      }
    |> ok
  in
  let _ =
    Mock_bank.step_up
      bank
      ~now:!now
      ~token
      { password = Mock_bank.demo_password; totp = "654321" }
    |> ok
  in
  let s = Mock_bank.execute_swap bank ~now:!now ~token s.id |> ok in
  print_s [%sexp (s.status : string)];
  balances bank ~token;
  [%expect
    {|
    ((id swap_8) (from_account_id acc_btc_cus_demo)
     (to_account_id acc_eur_cus_demo) (sell ((amount 0.01) (currency BTC)))
     (fee ((amount 0.00005) (currency BTC)))
     (buy ((amount 597.00) (currency EUR))) (price 60000.00)
     (expires_at 2025-10-14T08:20:30.000Z) (status quoted) (provider_sca ()))
    403 sca_required: confirm it is you: enter your password and a code
    executed
    EUR   available €2,786.00 held €18.90
    BTC   available 0.0325 BTC held 0 BTC
    ETH   available 1.2 ETH held 0 ETH
    USDC  available 500 USDC held 0 USDC
    USDT  available 0 USDT held 0 USDT
    |}]
;;

let%expect_test "sign up and verify" =
  let bank = Mock_bank.create ~now:!now in
  let r =
    Mock_bank.signup
      bank
      ~now:!now
      { email = "sam@example.com"
      ; password = "a long enough password"
      ; given_name = "Sam"
      ; family_name = "Example"
      ; date_of_birth = "1990-04-01"
      ; nationality = "DE"
      ; mobile = "+4915100000000"
      ; address =
          { line1 = "Street 1"
          ; line2 = None
          ; city = "Berlin"
          ; postal_code = Some "10115"
          ; country = "DE"
          }
      }
    |> ok
  in
  print_s [%sexp (r.customer_id : string)];
  let token =
    (Mock_bank.login
       bank
       ~now:!now
       { email = "sam@example.com"; password = "a long enough password"; totp = "123456" }
     |> ok)
      .token
  in
  List.iter [ "email"; "mobile"; "identity_document"; "liveness" ] ~f:(fun step ->
    let s = Mock_bank.kyc_start bank ~now:!now ~token { step } |> ok in
    let code = if String.equal s.kind "hosted" then "simulated" else "123456" in
    let st = Mock_bank.kyc_complete bank ~now:!now ~token { step; code } |> ok in
    printf
      "%s %s -> %s %s\n"
      step
      s.kind
      st.status
      (String.concat ~sep:"," st.steps_to_complete));
  print_s [%sexp (List.length (Mock_bank.accounts bank ~token |> ok) : int)];
  [%expect
    {|
    cus_7
    email code_sent -> in_progress mobile,identity_document,liveness
    mobile code_sent -> in_progress identity_document,liveness
    identity_document hosted -> in_progress liveness
    liveness hosted -> active
    5
    |}]
;;

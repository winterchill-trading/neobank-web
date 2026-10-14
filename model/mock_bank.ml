open! Core
open Neobank_api_types
module A = Api_types

type 'a r = ('a, Api_failure.t) Result.t

let fail ?challenge status error message =
  Error (Api_failure.create ?challenge ~status error message)
;;

let demo_email = "ada@example.com"
let demo_password = "correct horse battery"
let sca_window_ms = 5. *. Clock.minute_ms

(* Every currency's price in euro cents per whole unit. *)
let price_cents = function
  | "EUR" -> Z.of_int 100
  | "BTC" -> Z.of_int 6_000_000
  | "ETH" -> Z.of_int 240_000
  | "USDC" | "USDT" -> Z.of_int 92
  | _ -> Z.of_int 100
;;

let pow10 n = Z.pow (Z.of_int 10) n
let exponent c = (Currency.get c).exponent

(* Minor units of [currency] as euro cents, rounded down. *)
let eur_cents ~currency units =
  Z.div (Z.mul units (price_cents currency)) (pow10 (exponent currency))
;;

type customer =
  { id : string
  ; email : string
  ; password : string
  ; mutable kyc : A.Kyc.Status.t
  ; mutable sca_at : float
  ; mutable account_ids : string list
  }

type account =
  { info : A.Account.t
  ; owner : string
  ; mutable posted : Z.t
  ; mutable held : Z.t
  ; mutable lines : A.Statement.Line.t list (* newest first *)
  }

type card =
  { mutable card : A.Card.t
  ; card_owner : string
  ; pan : string
  ; mutable consent : [ `None | `Started of string | `Confirmed ]
  }

(* A challenge the mock is waiting on: ours on a transfer, or a provider code. *)
type challenge =
  { info : A.Sca_info.t
  ; on_success : unit -> unit
  }

type t =
  { mutable customers : customer list
  ; sessions : string String.Table.t
  ; accounts : account String.Table.t
  ; mutable beneficiaries : (string * A.Beneficiary.t) list
  ; mutable transfers : (string * A.Transfer.t) list (* newest first *)
  ; mutable swaps : (string * A.Swap.t) list
  ; mutable cards : card list
  ; mutable standing_orders : (string * A.Standing_order.t) list
  ; mutable reviews : A.Review.t list
  ; challenges : challenge String.Table.t
  ; mutable next : int
  }

let fresh_id t prefix =
  t.next <- t.next + 1;
  sprintf "%s_%d" prefix t.next
;;

(* {2 Sessions} *)

let customer_of_token t ~token =
  match Hashtbl.find t.sessions token with
  | None -> fail 401 "unauthorized" "sign in again"
  | Some id ->
    (match List.find t.customers ~f:(fun c -> String.equal c.id id) with
     | Some c -> Ok c
     | None -> fail 401 "unauthorized" "sign in again")
;;

let good_code code =
  match Check.totp code with
  | Ok "000000" | Error _ -> false
  | Ok _ -> true
;;

let require_fresh ~now (c : customer) =
  if Float.(now -. c.sca_at <= sca_window_ms)
  then Ok ()
  else fail 403 "sca_required" "confirm it is you: enter your password and a code"
;;

let require_active (c : customer) =
  if String.equal c.kyc.status "active"
  then Ok ()
  else fail 409 "kyc_incomplete" "finish verifying your identity first"
;;

let session t ~now (c : customer) : A.Auth.Session.t =
  let token = fresh_id t "mock-session" in
  Hashtbl.set t.sessions ~key:token ~data:c.id;
  c.sca_at <- now;
  { token; customer_id = c.id; idle_timeout_s = 300; sca_at = Clock.iso now }
;;

let new_challenge t ~now ~factors ~on_success =
  let id = fresh_id t "sca" in
  let info : A.Sca_info.t =
    { challenge_id = id
    ; factors
    ; expires_at = Clock.iso (now +. (5. *. Clock.minute_ms))
    ; attempts_left = 5
    }
  in
  Hashtbl.set t.challenges ~key:id ~data:{ info; on_success };
  info
;;

(* {2 Accounts} *)

let account_view (a : account) : A.Account.t =
  let currency = a.info.currency in
  let amount z = Units.to_amount ~currency z in
  { a.info with
    balance =
      Some
        { available = amount (Z.sub a.posted a.held)
        ; held = amount a.held
        ; posted = amount a.posted
        ; as_of =
            (match a.lines with
             | l :: _ -> l.at
             | [] -> "")
        }
  }
;;

let book (a : account) ~at ~code ~reference delta =
  a.posted <- Z.add a.posted delta;
  let currency = a.info.currency in
  a.lines
  <- { at
     ; amount = Units.to_amount ~currency delta
     ; code
     ; reference
     ; balance = Units.to_amount ~currency a.posted
     }
     :: a.lines
;;

let owned_account t (c : customer) id =
  match Hashtbl.find t.accounts id with
  | Some a when String.equal a.owner c.id -> Ok a
  | _ -> fail 404 "not_found" "no such account"
;;

let open_account t (c : customer) ~currency ~now =
  let id = sprintf "acc_%s_%s" (String.lowercase currency) c.id in
  let n = List.length t.customers in
  let info : A.Account.t =
    let base : A.Account.t =
      { id
      ; currency
      ; status = "active"
      ; iban = None
      ; bic = None
      ; address = None
      ; network = None
      ; lightning_node = None
      ; balance = None
      }
    in
    match currency with
    | "EUR" ->
      (* A valid German IBAN per customer: bank code 37040044 and a 10-digit account
         number from the customer count; the check digits are the ones Check.iban accepts. *)
      let bban = sprintf "37040044%010d" (532013000 + n) in
      let check =
        List.range 2 99
        |> List.find_exn ~f:(fun d ->
          Result.is_ok (Check.iban (sprintf "DE%02d%s" d bban)))
      in
      { base with iban = Some (sprintf "DE%02d%s" check bban); bic = Some "DEMODEFFXXX" }
    | "BTC" ->
      { base with
        address = Some "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
      ; network = Some "BTC"
      ; lightning_node =
          Some "03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f"
      }
    | "ETH" ->
      { base with
        address = Some "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
      ; network = Some "ETH"
      }
    | _ ->
      { base with
        address = Some "0x52908400098527886E0F7030069857D2E4169EE7"
      ; network = Some "ETH"
      }
  in
  let a = { info; owner = c.id; posted = Z.zero; held = Z.zero; lines = [] } in
  ignore now;
  Hashtbl.set t.accounts ~key:id ~data:a;
  c.account_ids <- c.account_ids @ [ id ];
  a
;;

(* {2 Seed} *)

let add_transfer t (c : customer) (tr : A.Transfer.t) =
  t.transfers <- (c.id, tr) :: t.transfers
;;

let create ~now =
  let t =
    { customers = []
    ; sessions = String.Table.create ()
    ; accounts = String.Table.create ()
    ; beneficiaries = []
    ; transfers = []
    ; swaps = []
    ; cards = []
    ; standing_orders = []
    ; reviews = []
    ; challenges = String.Table.create ()
    ; next = 0
    }
  in
  let ada =
    { id = "cus_ada"
    ; email = demo_email
    ; password = demo_password
    ; kyc =
        { status = "active"
        ; tier = "T1"
        ; steps_to_complete = []
        ; fields_to_resubmit = []
        }
    ; sca_at = Float.neg_infinity
    ; account_ids = []
    }
  in
  t.customers <- [ ada ];
  let ago days = Clock.iso (now -. (days *. Clock.day_ms)) in
  let eur = open_account t ada ~currency:"EUR" ~now in
  let btc = open_account t ada ~currency:"BTC" ~now in
  let eth = open_account t ada ~currency:"ETH" ~now in
  let usdc = open_account t ada ~currency:"USDC" ~now in
  let _usdt = open_account t ada ~currency:"USDT" ~now in
  let units currency text = Units.parse ~currency text |> Or_error.ok_exn in
  let landlord : A.Beneficiary.t =
    { id = "ben_landlord"
    ; name = "Grace Hopper"
    ; rails = [ "sepa" ]
    ; destination = "GB82WEST12345698765432"
    ; trusted = true
    ; payee_check = Some "match"
    ; created_at = ago 40.
    }
  in
  let cold : A.Beneficiary.t =
    { id = "ben_cold"
    ; name = "Cold storage"
    ; rails = [ "onchain:btc" ]
    ; destination = "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
    ; trusted = false
    ; payee_check = None
    ; created_at = ago 20.
    }
  in
  t.beneficiaries <- [ ada.id, landlord; ada.id, cold ];
  let movement (a : account) ~days ~code ~reference ~counterparty ~rail ~direction text =
    let delta = units a.info.currency text in
    let delta = if String.equal direction "out" then Z.neg delta else delta in
    let at = ago days in
    book a ~at ~code ~reference delta;
    add_transfer
      t
      ada
      { id = fresh_id t "tr"
      ; account_id = a.info.id
      ; beneficiary_id = None
      ; counterparty
      ; direction
      ; rail
      ; amount = Units.to_amount ~currency:a.info.currency (Z.abs delta)
      ; fee = Units.to_amount ~currency:a.info.currency Z.zero
      ; reference
      ; status = "settled"
      ; under_review = false
      ; sca = None
      ; provider_sca = None
      ; message = None
      ; created_at = at
      ; updated_at = at
      }
  in
  movement
    eur
    ~days:25.
    ~code:"sepa_in"
    ~reference:"Salary September"
    ~counterparty:"Analytical Engines Ltd"
    ~rail:"sepa"
    ~direction:"in"
    "3200.00";
  movement
    eur
    ~days:24.
    ~code:"sepa_out"
    ~reference:"Rent October"
    ~counterparty:"Grace Hopper"
    ~rail:"sepa"
    ~direction:"out"
    "950.00";
  movement
    btc
    ~days:12.
    ~code:"onchain_in"
    ~reference:"From exchange"
    ~counterparty:"bc1q...7k2f"
    ~rail:"onchain:btc"
    ~direction:"in"
    "0.0425";
  movement
    eth
    ~days:9.
    ~code:"onchain_in"
    ~reference:"Deposit"
    ~counterparty:"0x71C7...976F"
    ~rail:"onchain:eth"
    ~direction:"in"
    "1.2";
  movement
    usdc
    ~days:5.
    ~code:"onchain_in"
    ~reference:"Deposit"
    ~counterparty:"0x8ba1...BA72"
    ~rail:"onchain:eth"
    ~direction:"in"
    "500";
  movement
    eur
    ~days:2.
    ~code:"sepa_out"
    ~reference:"Groceries"
    ~counterparty:"Corner Shop"
    ~rail:"sepa_instant"
    ~direction:"out"
    "42.10";
  (* A card authorisation waiting to clear: held, not yet posted. *)
  eur.held <- units "EUR" "18.90";
  t.cards
  <- [ { card =
           { id = "card_1"
           ; account_id = eur.info.id
           ; kind = "virtual"
           ; status = "active"
           ; masked_pan = Some "424242******4242"
           ; limits =
               { per_transaction =
                   Some (Units.to_amount ~currency:"EUR" (units "EUR" "2000"))
               ; daily = Some (Units.to_amount ~currency:"EUR" (units "EUR" "3000"))
               ; monthly = None
               ; online = true
               ; contactless = true
               ; atm = false
               }
           ; created_at = ago 30.
           }
       ; card_owner = ada.id
       ; pan = "4242424242424242"
       ; consent = `None
       }
     ];
  t.standing_orders
  <- [ ( ada.id
       , { id = "so_rent"
         ; account_id = eur.info.id
         ; beneficiary_id = landlord.id
         ; amount = { amount = "950.00"; currency = "EUR" }
         ; schedule = "monthly:1"
         ; next_run = Clock.date (now +. (7. *. Clock.day_ms))
         ; status = "active"
         ; runs = 3
         ; provider_sca = None
         } )
     ];
  t
;;

(* {2 Sessions and KYC} *)

let signup t ~now (r : A.Auth.Signup_request.t) =
  let open Result.Let_syntax in
  let validation x =
    Result.map_error x ~f:(fun e ->
      Api_failure.create ~status:400 "invalid_request" (Error.to_string_hum e))
  in
  let%bind email = validation (Check.email r.email) in
  let%bind _ = validation (Check.password r.password) in
  let%bind _ = validation (Check.date r.date_of_birth) in
  if List.exists t.customers ~f:(fun c -> String.Caseless.equal c.email email)
  then fail 409 "email_taken" "an account with this email exists; sign in instead"
  else (
    let id = fresh_id t "cus" in
    let c =
      { id
      ; email
      ; password = r.password
      ; kyc =
          { status = "in_progress"
          ; tier = "T0"
          ; steps_to_complete = [ "email"; "mobile"; "identity_document"; "liveness" ]
          ; fields_to_resubmit = []
          }
      ; sca_at = Float.neg_infinity
      ; account_ids = []
      }
    in
    ignore now;
    t.customers <- t.customers @ [ c ];
    Ok
      { A.Auth.Signup_response.customer_id = id
      ; totp_secret = "JBSWY3DPEHPK3PXP"
      ; otpauth_uri =
          sprintf
            "otpauth://totp/Neobank%%20demo:%s?secret=JBSWY3DPEHPK3PXP&issuer=Neobank%%20demo"
            email
      })
;;

let login t ~now (r : A.Auth.Login_request.t) =
  match
    List.find t.customers ~f:(fun c ->
      String.Caseless.equal c.email (String.strip r.email))
  with
  | Some c when String.equal c.password r.password && good_code r.totp ->
    Ok (session t ~now c)
  | _ -> fail 401 "invalid_credentials" "the email, password or code is wrong"
;;

let step_up t ~now ~token (r : A.Auth.Step_up_request.t) =
  let%bind.Result c = customer_of_token t ~token in
  if String.equal c.password r.password && good_code r.totp
  then (
    c.sca_at <- now;
    Ok
      { A.Auth.Session.token
      ; customer_id = c.id
      ; idle_timeout_s = 300
      ; sca_at = Clock.iso now
      })
  else fail 401 "invalid_credentials" "the password or code is wrong"
;;

let logout t ~token =
  Hashtbl.remove t.sessions token;
  Ok ()
;;

let me t ~now ~token =
  let%map.Result c = customer_of_token t ~token in
  { A.Me.customer_id = c.id
  ; email = c.email
  ; kyc = c.kyc
  ; accounts = List.length c.account_ids
  ; sca_fresh = Float.(now -. c.sca_at <= sca_window_ms)
  }
;;

let kyc_status t ~token =
  let%map.Result c = customer_of_token t ~token in
  c.kyc
;;

let hosted_steps = [ "identity_document"; "liveness"; "address_proof" ]

let kyc_start t ~now ~token (r : A.Kyc.Start_request.t) =
  let%bind.Result c = customer_of_token t ~token in
  if not (List.mem c.kyc.steps_to_complete r.step ~equal:String.equal)
  then fail 409 "step_not_open" "this step is not waiting for you"
  else (
    let expires_at = Clock.iso (now +. (10. *. Clock.minute_ms)) in
    if List.mem hosted_steps r.step ~equal:String.equal
    then
      Ok
        { A.Kyc.Session.step = r.step
        ; kind = "hosted"
        ; sdk_token = Some (fresh_id t "mock-sdk-token")
        ; expires_at
        }
    else Ok { step = r.step; kind = "code_sent"; sdk_token = None; expires_at })
;;

let currencies = [ "EUR"; "BTC"; "ETH"; "USDC"; "USDT" ]

let kyc_complete t ~now ~token (r : A.Kyc.Complete_request.t) =
  let%bind.Result c = customer_of_token t ~token in
  let hosted = List.mem hosted_steps r.step ~equal:String.equal in
  let accepted = if hosted then String.equal r.code "simulated" else good_code r.code in
  if not (List.mem c.kyc.steps_to_complete r.step ~equal:String.equal)
  then fail 409 "step_not_open" "this step is not waiting for you"
  else if not accepted
  then fail 422 "wrong_code" "the code is wrong or expired"
  else (
    let steps = List.filter c.kyc.steps_to_complete ~f:(Fn.non (String.equal r.step)) in
    c.kyc
    <- (if List.is_empty steps
        then { c.kyc with status = "active"; tier = "T1"; steps_to_complete = [] }
        else { c.kyc with steps_to_complete = steps });
    if List.is_empty steps && List.is_empty c.account_ids
    then
      List.iter currencies ~f:(fun currency ->
        ignore (open_account t c ~currency ~now : account));
    Ok c.kyc)
;;

let kyc_decision t ~now ~token (d : A.Sandbox.Kyc_decision.t) =
  let%bind.Result c = customer_of_token t ~token in
  match d.decision with
  | "approve" ->
    c.kyc <- { c.kyc with status = "active"; tier = "T1"; steps_to_complete = [] };
    if List.is_empty c.account_ids
    then
      List.iter currencies ~f:(fun currency ->
        ignore (open_account t c ~currency ~now : account));
    Ok c.kyc
  | "reject" ->
    c.kyc <- { c.kyc with status = "declined"; steps_to_complete = [] };
    Ok c.kyc
  | "resubmit" ->
    c.kyc <- { c.kyc with status = "action_required"; fields_to_resubmit = [ "address" ] };
    Ok c.kyc
  | _ -> fail 400 "invalid_request" "decision is approve, reject or resubmit"
;;

(* {2 Accounts} *)

let accounts t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map c.account_ids ~f:(fun id ->
    Hashtbl.find t.accounts id |> Option.map ~f:account_view)
;;

let account t ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%map.Result a = owned_account t c id in
  account_view a
;;

let statement t ~now ~token ?from ?until id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result a = owned_account t c id in
  let from = Option.value from ~default:(Clock.date (now -. (30. *. Clock.day_ms))) in
  let until = Option.value until ~default:(Clock.date now) in
  let in_range (l : A.Statement.Line.t) =
    let d = Format.date l.at in
    String.( >= ) d from && String.( <= ) d until
  in
  let lines = List.rev a.lines in
  let before = List.filter lines ~f:(fun l -> String.( < ) (Format.date l.at) from) in
  let inside = List.filter lines ~f:in_range in
  let currency = a.info.currency in
  let zero = Units.to_amount ~currency Z.zero in
  let opening =
    Option.value_map (List.last before) ~default:zero ~f:(fun l -> l.balance)
  in
  let closing =
    Option.value_map (List.last inside) ~default:opening ~f:(fun l -> l.balance)
  in
  Ok
    { A.Statement.account_id = id
    ; currency
    ; from = from ^ "T00:00:00.000Z"
    ; until = until ^ "T23:59:59.999Z"
    ; opening
    ; closing
    ; lines = inside
    }
;;

let parse_amount ~currency (a : A.Amount.t) =
  if not (String.equal a.currency currency)
  then
    fail
      400
      "invalid_request"
      (sprintf "the account holds %s, not %s" currency a.currency)
  else
    Result.map_error (Units.parse_positive ~currency a.amount) ~f:(fun e ->
      Api_failure.create ~status:400 "invalid_request" (Error.to_string_hum e))
;;

let deposit t ~now ~token (d : A.Sandbox.Deposit.t) =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result a = owned_account t c d.account_id in
  let currency = a.info.currency in
  let%bind.Result units = parse_amount ~currency d.amount in
  let rail =
    Option.value
      d.rail
      ~default:
        (if String.equal currency "EUR"
         then "sepa_instant"
         else
           "onchain:" ^ String.lowercase (Option.value a.info.network ~default:currency))
  in
  let at = Clock.iso now in
  book a ~at ~code:"deposit" ~reference:"Sandbox deposit" units;
  let tr : A.Transfer.t =
    { id = fresh_id t "tr"
    ; account_id = a.info.id
    ; beneficiary_id = None
    ; counterparty = "Sandbox deposit"
    ; direction = "in"
    ; rail
    ; amount = Units.to_amount ~currency units
    ; fee = Units.to_amount ~currency Z.zero
    ; reference = "Sandbox deposit"
    ; status = "settled"
    ; under_review = false
    ; sca = None
    ; provider_sca = None
    ; message = None
    ; created_at = at
    ; updated_at = at
    }
  in
  add_transfer t c tr;
  Ok tr
;;

(* {2 Payments} *)

let beneficiaries t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map t.beneficiaries ~f:(fun (owner, b) ->
    Option.some_if (String.equal owner c.id) b)
;;

let rail_network rail = String.chop_prefix rail ~prefix:"onchain:"

let check_destination ~rail destination =
  let network =
    match rail with
    | "sepa" | "sepa_instant" -> None
    | "lightning" -> Some "LIGHTNING"
    | r -> Option.map (rail_network r) ~f:String.uppercase
  in
  let checked =
    match network with
    | None -> Check.iban destination
    | Some network -> Check.address ~network destination
  in
  Result.map_error checked ~f:(fun e ->
    Api_failure.create ~status:400 "invalid_request" (Error.to_string_hum e))
;;

let add_beneficiary t ~now ~token (r : A.Beneficiary.Request.t) =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result destination = check_destination ~rail:r.rail r.destination in
  let b : A.Beneficiary.t =
    { id = fresh_id t "ben"
    ; name = String.strip r.name
    ; rails = [ r.rail ]
    ; destination
    ; trusted = false
    ; payee_check =
        (match r.rail with
         | "sepa" | "sepa_instant" ->
           Some
             (if String.is_prefix destination ~prefix:"DE"
              then "match"
              else "not_possible")
         | _ -> None)
    ; created_at = Clock.iso now
    }
  in
  t.beneficiaries <- t.beneficiaries @ [ c.id, b ];
  Ok b
;;

let owned_beneficiary t (c : customer) id =
  match
    List.find t.beneficiaries ~f:(fun (owner, b) ->
      String.equal owner c.id && String.equal b.id id)
  with
  | Some (_, b) -> Ok b
  | None -> fail 404 "not_found" "no such payee"
;;

let trust_beneficiary t ~now ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result () = require_fresh ~now c in
  let%bind.Result b = owned_beneficiary t c id in
  let b = { b with trusted = true } in
  t.beneficiaries
  <- List.map t.beneficiaries ~f:(fun (o, x) ->
       if String.equal x.id id then o, b else o, x);
  Ok b
;;

let fee_for ~rail ~currency =
  let u text = Units.parse ~currency text |> Or_error.ok_exn in
  match rail, currency with
  | "sepa", _ -> Z.zero
  | "sepa_instant", _ -> u "0.25"
  | "lightning", _ -> u "0.0000001"
  | _, "BTC" -> u "0.00002"
  | _, "ETH" -> u "0.0004"
  | _, _ -> u "1"
;;

let default_rail (a : account) =
  match a.info.currency with
  | "EUR" -> "sepa"
  | _ ->
    "onchain:" ^ String.lowercase (Option.value a.info.network ~default:a.info.currency)
;;

let fee_quote t ~token (r : A.Fee_quote.Request.t) =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result a = owned_account t c r.account_id in
  let currency = a.info.currency in
  let%bind.Result amount = parse_amount ~currency r.amount in
  let rail = if String.is_empty r.rail then default_rail a else r.rail in
  let fee = fee_for ~rail ~currency in
  Ok
    { A.Fee_quote.rail
    ; amount = Units.to_amount ~currency amount
    ; fee = Units.to_amount ~currency fee
    ; total = Units.to_amount ~currency (Z.add amount fee)
    }
;;

let review_threshold_cents = Z.of_int 100_000

let replace_transfer t (tr : A.Transfer.t) =
  t.transfers
  <- List.map t.transfers ~f:(fun (o, x) ->
       if String.equal x.id tr.id then o, tr else o, x)
;;

let find_transfer t id = List.find t.transfers ~f:(fun (_, x) -> String.equal x.id id)

let settle t ~now (tr : A.Transfer.t) =
  match Hashtbl.find t.accounts tr.account_id with
  | None -> tr
  | Some a ->
    let amount = Units.of_amount tr.amount |> Or_error.ok_exn in
    let fee = Units.of_amount tr.fee |> Or_error.ok_exn in
    let total = Z.add amount fee in
    a.held <- Z.max Z.zero (Z.sub a.held total);
    let at = Clock.iso now in
    book
      a
      ~at
      ~code:(String.substr_replace_all tr.rail ~pattern:":" ~with_:"_" ^ "_out")
      ~reference:tr.reference
      (Z.neg total);
    let tr =
      { tr with status = "settled"; sca = None; provider_sca = None; updated_at = at }
    in
    replace_transfer t tr;
    tr
;;

(* Our challenge on a transfer: password and an authenticator code. On-chain payments then
   get a provider code too, as the real provider asks for one. *)
let ask_sca t ~now (tr : A.Transfer.t) =
  let info =
    new_challenge t ~now ~factors:[ "knowledge"; "possession" ] ~on_success:(fun () ->
      match find_transfer t tr.id with
      | None -> ()
      | Some (_, tr) ->
        if Option.is_some (rail_network tr.rail)
        then (
          let provider =
            new_challenge t ~now ~factors:[ "possession" ] ~on_success:(fun () ->
              match find_transfer t tr.id with
              | Some (_, tr) -> ignore (settle t ~now tr : A.Transfer.t)
              | None -> ())
          in
          replace_transfer
            t
            { tr with status = "submitted"; sca = None; provider_sca = Some provider })
        else ignore (settle t ~now tr : A.Transfer.t))
  in
  { tr with status = "awaiting_sca"; sca = Some info }
;;

let create_transfer t ~now ~token (r : A.Transfer.Request.t) =
  let open Result.Let_syntax in
  let%bind c = customer_of_token t ~token in
  let%bind () = require_active c in
  let%bind a = owned_account t c r.account_id in
  let%bind b = owned_beneficiary t c r.beneficiary_id in
  let currency = a.info.currency in
  let%bind amount = parse_amount ~currency r.amount in
  let rail =
    Option.value
      r.rail
      ~default:(List.hd b.rails |> Option.value ~default:(default_rail a))
  in
  let fee = fee_for ~rail ~currency in
  let total = Z.add amount fee in
  let available = Z.sub a.posted a.held in
  if Z.gt total available
  then
    fail 422 "insufficient_funds" "the amount and fee are more than the available balance"
  else (
    a.held <- Z.add a.held total;
    let at = Clock.iso now in
    let tr : A.Transfer.t =
      { id = fresh_id t "tr"
      ; account_id = a.info.id
      ; beneficiary_id = Some b.id
      ; counterparty = b.name
      ; direction = "out"
      ; rail
      ; amount = Units.to_amount ~currency amount
      ; fee = Units.to_amount ~currency fee
      ; reference = Option.value r.reference ~default:""
      ; status = "draft"
      ; under_review = false
      ; sca = None
      ; provider_sca = None
      ; message = None
      ; created_at = at
      ; updated_at = at
      }
    in
    let tr =
      if Z.gt (eur_cents ~currency amount) review_threshold_cents
      then (
        t.reviews
        <- t.reviews
           @ [ { id = fresh_id t "rev"
               ; reference = tr.id
               ; customer_id = c.id
               ; severity = "review"
               ; reasons = [ "large_amount: above the 1,000 EUR review threshold" ]
               ; submitted_at = at
               ; status = "pending"
               ; operator = None
               }
             ];
        { tr with
          status = "under_review"
        ; under_review = true
        ; message = Some "under review"
        })
      else ask_sca t ~now tr
    in
    add_transfer t c tr;
    Ok tr)
;;

let transfers t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map t.transfers ~f:(fun (o, x) -> Option.some_if (String.equal o c.id) x)
;;

let owned_transfer t (c : customer) id =
  match find_transfer t id with
  | Some (o, x) when String.equal o c.id -> Ok x
  | _ -> fail 404 "not_found" "no such payment"
;;

let transfer t ~token id =
  let%bind.Result c = customer_of_token t ~token in
  owned_transfer t c id
;;

let answer t ~now ~challenge_id ~code ~ok_extra =
  match Hashtbl.find t.challenges challenge_id with
  | None -> fail 409 "no_challenge" "nothing is waiting for a code"
  | Some ch when String.( < ) ch.info.expires_at (Clock.iso now) ->
    Hashtbl.remove t.challenges challenge_id;
    fail 410 "challenge_expired" "the code expired; start again"
  | Some ch ->
    if good_code code && ok_extra
    then (
      Hashtbl.remove t.challenges challenge_id;
      ch.on_success ();
      Ok ())
    else (
      let attempts_left = ch.info.attempts_left - 1 in
      if attempts_left <= 0
      then (
        Hashtbl.remove t.challenges challenge_id;
        fail 423 "locked" "too many wrong codes")
      else (
        let info = { ch.info with attempts_left } in
        Hashtbl.set t.challenges ~key:challenge_id ~data:{ ch with info };
        fail ~challenge:info 422 "wrong_code" "the code is wrong"))
;;

let confirm_sca t ~now ~token id (r : A.Transfer.Confirm_request.t) =
  let open Result.Let_syntax in
  let%bind c = customer_of_token t ~token in
  let%bind tr = owned_transfer t c id in
  let%bind () =
    match tr.sca, tr.provider_sca with
    | Some ours, _ ->
      let password_ok =
        Option.value_map r.password ~default:false ~f:(String.equal c.password)
      in
      let result =
        answer t ~now ~challenge_id:ours.challenge_id ~code:r.code ~ok_extra:password_ok
      in
      (* Keep the transfer's copy of the challenge in step with the attempts left. *)
      (match result with
       | Error { challenge = Some info; _ } ->
         replace_transfer t { tr with sca = Some info }
       | _ -> ());
      result
    | None, Some provider ->
      let result =
        answer t ~now ~challenge_id:provider.challenge_id ~code:r.code ~ok_extra:true
      in
      (match result with
       | Error { challenge = Some info; _ } ->
         replace_transfer t { tr with provider_sca = Some info }
       | _ -> ());
      result
    | None, None -> fail 409 "no_challenge" "this payment is not waiting for a code"
  in
  owned_transfer t c id
;;

let restart_sca t ~now ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result tr = owned_transfer t c id in
  match tr.status with
  | "awaiting_sca" when Option.is_some tr.sca || Option.is_none tr.provider_sca ->
    Option.iter tr.sca ~f:(fun s -> Hashtbl.remove t.challenges s.challenge_id);
    let tr = ask_sca t ~now tr in
    replace_transfer t tr;
    Ok tr
  | _ -> fail 409 "no_challenge" "this payment is not waiting for a code"
;;

let cancel_transfer t ~now ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result tr = owned_transfer t c id in
  match tr.status with
  | "awaiting_sca" | "under_review" | "draft" ->
    (match Hashtbl.find t.accounts tr.account_id with
     | Some a ->
       let total =
         Z.add
           (Units.of_amount tr.amount |> Or_error.ok_exn)
           (Units.of_amount tr.fee |> Or_error.ok_exn)
       in
       a.held <- Z.max Z.zero (Z.sub a.held total)
     | None -> ());
    let tr =
      { tr with
        status = "cancelled"
      ; sca = None
      ; under_review = false
      ; updated_at = Clock.iso now
      }
    in
    replace_transfer t tr;
    Ok tr
  | _ -> fail 409 "too_late" "the payment was already sent"
;;

(* {2 Swaps} *)

let swap_quote t ~now ~token (r : A.Swap.Quote_request.t) =
  let open Result.Let_syntax in
  let%bind c = customer_of_token t ~token in
  let%bind () = require_active c in
  let%bind from = owned_account t c r.from_account_id in
  let%bind to_ = owned_account t c r.to_account_id in
  let from_c = from.info.currency
  and to_c = to_.info.currency in
  if String.equal from_c to_c
  then fail 400 "invalid_request" "pick two different currencies"
  else (
    let%bind sell = parse_amount ~currency:from_c r.sell in
    if Z.gt sell (Z.sub from.posted from.held)
    then fail 422 "insufficient_funds" "more than the available balance"
    else (
      (* 0.5% fee in the sold currency, then the rest at the fixed mock price. *)
      let fee = Z.div sell (Z.of_int 200) in
      let net = Z.sub sell fee in
      let buy =
        Z.div
          (Z.mul (Z.mul net (price_cents from_c)) (pow10 (exponent to_c)))
          (Z.mul (price_cents to_c) (pow10 (exponent from_c)))
      in
      let price =
        Units.to_string
          ~currency:to_c
          (Z.div (Z.mul (price_cents from_c) (pow10 (exponent to_c))) (price_cents to_c))
      in
      let s : A.Swap.t =
        { id = fresh_id t "swap"
        ; from_account_id = from.info.id
        ; to_account_id = to_.info.id
        ; sell = Units.to_amount ~currency:from_c sell
        ; fee = Units.to_amount ~currency:from_c fee
        ; buy = Units.to_amount ~currency:to_c buy
        ; price
        ; expires_at = Clock.iso (now +. 30_000.)
        ; status = "quoted"
        ; provider_sca = None
        }
      in
      t.swaps <- (c.id, s) :: t.swaps;
      Ok s))
;;

let replace_swap t (s : A.Swap.t) =
  t.swaps
  <- List.map t.swaps ~f:(fun (o, x) -> if String.equal x.id s.id then o, s else o, x)
;;

let execute_swap t ~now ~token id =
  let open Result.Let_syntax in
  let%bind c = customer_of_token t ~token in
  let%bind () = require_fresh ~now c in
  let%bind s =
    match
      List.find t.swaps ~f:(fun (o, x) -> String.equal o c.id && String.equal x.id id)
    with
    | Some (_, s) -> Ok s
    | None -> fail 404 "not_found" "no such swap"
  in
  if not (String.equal s.status "quoted")
  then fail 409 "not_quoted" "this quote was already used"
  else if String.( < ) s.expires_at (Clock.iso now)
  then fail 410 "quote_expired" "the quote expired; ask for a new one"
  else (
    let%bind from = owned_account t c s.from_account_id in
    let%bind to_ = owned_account t c s.to_account_id in
    let sell = Units.of_amount s.sell |> Or_error.ok_exn in
    let buy = Units.of_amount s.buy |> Or_error.ok_exn in
    let at = Clock.iso now in
    book
      from
      ~at
      ~code:"swap_out"
      ~reference:("Swap to " ^ to_.info.currency)
      (Z.neg sell);
    book to_ ~at ~code:"swap_in" ~reference:("Swap from " ^ from.info.currency) buy;
    let s = { s with status = "executed" } in
    replace_swap t s;
    Ok s)
;;

let swaps t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map t.swaps ~f:(fun (o, x) -> Option.some_if (String.equal o c.id) x)
;;

(* {2 Cards} *)

let owned_card t (c : customer) id =
  match
    List.find t.cards ~f:(fun k ->
      String.equal k.card_owner c.id && String.equal k.card.id id)
  with
  | Some k -> Ok k
  | None -> fail 404 "not_found" "no such card"
;;

let cards t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map t.cards ~f:(fun k ->
    Option.some_if (String.equal k.card_owner c.id) k.card)
;;

let issue_card t ~now ~token (r : A.Card.Issue_request.t) =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result () = require_active c in
  let%bind.Result a = owned_account t c r.account_id in
  let n = List.length t.cards + 1 in
  let last4 = sprintf "%04d" (1000 + (n * 37)) in
  let card : A.Card.t =
    { id = fresh_id t "card"
    ; account_id = a.info.id
    ; kind = "virtual"
    ; status = "active"
    ; masked_pan = Some ("424242******" ^ last4)
    ; limits =
        { per_transaction = Some { amount = "1000.00"; currency = "EUR" }
        ; daily = Some { amount = "2000.00"; currency = "EUR" }
        ; monthly = Some { amount = "5000.00"; currency = "EUR" }
        ; online = true
        ; contactless = true
        ; atm = false
        }
    ; created_at = Clock.iso now
    }
  in
  t.cards
  <- t.cards
     @ [ { card; card_owner = c.id; pan = "424242000000" ^ last4; consent = `None } ];
  Ok card
;;

let set_status t ~token id ~from ~to_ =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result k = owned_card t c id in
  if String.equal k.card.status from
  then (
    k.card <- { k.card with status = to_ };
    Ok k.card)
  else fail 409 "invalid_state" (sprintf "the card is %s" k.card.status)
;;

let freeze_card t ~token id = set_status t ~token id ~from:"active" ~to_:"frozen"
let unfreeze_card t ~token id = set_status t ~token id ~from:"frozen" ~to_:"active"

let set_card_limits t ~now ~token id (l : A.Card.Limits.t) =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result () = require_fresh ~now c in
  let%bind.Result k = owned_card t c id in
  let check (a : A.Amount.t option) =
    match a with
    | None -> Ok ()
    | Some a -> Result.map (parse_amount ~currency:"EUR" a) ~f:ignore
  in
  let%bind.Result () = check l.per_transaction in
  let%bind.Result () = check l.daily in
  let%bind.Result () = check l.monthly in
  k.card <- { k.card with limits = l };
  Ok k.card
;;

let card_display t ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%map.Result k = owned_card t c id in
  { Hosted.Card_display.provider = "simulated"
  ; script_url = None
  ; application_id = "mock-application"
  ; ui_secret = "mock-ui-secret"
  ; user_id = c.id
  ; card_id = k.card.id
  }
;;

let card_consent t ~now ~token id ~challenge_id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result k = owned_card t c id in
  let info =
    new_challenge t ~now ~factors:[ "possession" ] ~on_success:(fun () ->
      k.consent <- `Confirmed)
  in
  ignore challenge_id;
  k.consent <- `Started info.challenge_id;
  Ok info
;;

let pan_token t ~now ~token id =
  let%bind.Result c = customer_of_token t ~token in
  let%bind.Result () = require_fresh ~now c in
  let%bind.Result k = owned_card t c id in
  match k.consent with
  | `Confirmed ->
    Ok
      (`Token
        { A.Card.Pan_token.token = fresh_id t "mock-card-auth"
        ; expires_at = Clock.iso (now +. Clock.day_ms)
        })
  | `Started challenge_id ->
    (match Hashtbl.find t.challenges challenge_id with
     | Some ch -> Ok (`Challenge ch.info)
     | None -> fail 409 "consent_required" "start the card display again")
  | `None -> fail 409 "consent_required" "the card display has to ask for consent first"
;;

let simulated_pan t id =
  List.find_map t.cards ~f:(fun k -> Option.some_if (String.equal k.card.id id) k.pan)
;;

(* {2 Standing orders} *)

let standing_orders t ~token =
  let%map.Result c = customer_of_token t ~token in
  List.filter_map t.standing_orders ~f:(fun (o, x) ->
    Option.some_if (String.equal o c.id) x)
;;

let create_standing_order t ~now ~token (r : A.Standing_order.Request.t) =
  let open Result.Let_syntax in
  let%bind c = customer_of_token t ~token in
  let%bind () = require_active c in
  let%bind a = owned_account t c r.account_id in
  let%bind _ = owned_beneficiary t c r.beneficiary_id in
  let%bind amount = parse_amount ~currency:a.info.currency r.amount in
  let%bind start =
    Result.map_error (Check.date r.start) ~f:(fun e ->
      Api_failure.create ~status:400 "invalid_request" (Error.to_string_hum e))
  in
  let%bind () =
    match String.lsplit2 r.schedule ~on:':' with
    | None when String.equal r.schedule "daily" -> Ok ()
    | Some ("weekly", ("mon" | "tue" | "wed" | "thu" | "fri" | "sat" | "sun")) -> Ok ()
    | Some ("monthly", d)
      when Option.exists (Int.of_string_opt d) ~f:(fun d -> d >= 1 && d <= 28) -> Ok ()
    | _ -> fail 400 "invalid_request" "unknown schedule (daily, weekly:mon, monthly:15)"
  in
  if String.( < ) start (Clock.date now)
  then fail 400 "invalid_request" "the first payment cannot be in the past"
  else (
    let o : A.Standing_order.t =
      { id = fresh_id t "so"
      ; account_id = a.info.id
      ; beneficiary_id = r.beneficiary_id
      ; amount = Units.to_amount ~currency:a.info.currency amount
      ; schedule = r.schedule
      ; next_run = start
      ; status = "awaiting_sca"
      ; runs = 0
      ; provider_sca = None
      }
    in
    (* The provider confirms a new standing order with its own code. *)
    let info =
      new_challenge t ~now ~factors:[ "possession" ] ~on_success:(fun () ->
        t.standing_orders
        <- List.map t.standing_orders ~f:(fun (owner, x) ->
             if String.equal x.id o.id
             then owner, { x with status = "active"; provider_sca = None }
             else owner, x))
    in
    let o = { o with provider_sca = Some info } in
    t.standing_orders <- t.standing_orders @ [ c.id, o ];
    Ok o)
;;

let cancel_standing_order t ~token id =
  let%bind.Result c = customer_of_token t ~token in
  match
    List.find t.standing_orders ~f:(fun (o, x) ->
      String.equal o c.id && String.equal x.id id)
  with
  | None -> fail 404 "not_found" "no such standing order"
  | Some (_, o) ->
    let o = { o with status = "cancelled"; provider_sca = None } in
    t.standing_orders
    <- List.map t.standing_orders ~f:(fun (owner, x) ->
         if String.equal x.id id then owner, o else owner, x);
    Ok o
;;

let provider_sca t ~now ~token id (r : A.Sca_confirm_request.t) =
  let%bind.Result _ = customer_of_token t ~token in
  answer t ~now ~challenge_id:id ~code:r.code ~ok_extra:true
;;

(* {2 Operators} *)

let reviews t = Ok t.reviews

let decide_review t ~now id (r : A.Review.Decide_request.t) =
  match List.find t.reviews ~f:(fun x -> String.equal x.id id) with
  | None -> fail 404 "not_found" "no such review"
  | Some rv when not (String.equal rv.status "pending") ->
    fail 409 "decided" "already decided"
  | Some rv ->
    let approve =
      match r.verdict with
      | "approve" -> Some true
      | "reject" -> Some false
      | _ -> None
    in
    (match approve with
     | None -> fail 400 "invalid_request" "verdict is approve or reject"
     | Some approve ->
       let rv =
         { rv with
           status = (if approve then "approved" else "rejected")
         ; operator = Some r.operator
         }
       in
       t.reviews
       <- List.map t.reviews ~f:(fun x -> if String.equal x.id id then rv else x);
       (match find_transfer t rv.reference with
        | None -> ()
        | Some (_, tr) ->
          if approve
          then
            replace_transfer
              t
              (ask_sca t ~now { tr with under_review = false; message = None })
          else (
            (match Hashtbl.find t.accounts tr.account_id with
             | Some a ->
               let total =
                 Z.add
                   (Units.of_amount tr.amount |> Or_error.ok_exn)
                   (Units.of_amount tr.fee |> Or_error.ok_exn)
               in
               a.held <- Z.max Z.zero (Z.sub a.held total)
             | None -> ());
            replace_transfer
              t
              { tr with
                status = "cancelled"
              ; under_review = false
              ; message = Some "this payment could not be made"
              ; updated_at = Clock.iso now
              }));
       Ok rv)
;;

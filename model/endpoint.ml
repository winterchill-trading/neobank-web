open! Core

let esc s =
  String.concat_map s ~f:(fun c ->
    if Char.is_alphanum c || List.mem [ '-'; '_'; '.'; '~' ] c ~equal:Char.equal
    then String.of_char c
    else sprintf "%%%02X" (Char.to_int c))
;;

let signup = "/auth/signup"
let login = "/auth/login"
let step_up = "/auth/step-up"
let logout = "/auth/logout"
let me = "/me"
let kyc = "/kyc"
let kyc_start = "/kyc/start"
let kyc_complete = "/kyc/complete"
let accounts = "/accounts"
let account id = "/accounts/" ^ esc id

let statement ?from ?until id =
  let query =
    List.filter_opt
      [ Option.map from ~f:(fun d -> "from=" ^ esc d)
      ; Option.map until ~f:(fun d -> "until=" ^ esc d)
      ]
  in
  account id
  ^ "/statement"
  ^ if List.is_empty query then "" else "?" ^ String.concat ~sep:"&" query
;;

let beneficiaries = "/beneficiaries"
let trust_beneficiary id = "/beneficiaries/" ^ esc id ^ "/trust"
let fee_quote = "/transfers/quote"
let transfers = "/transfers"
let transfer id = "/transfers/" ^ esc id
let confirm_sca id = transfer id ^ "/confirm-sca"
let cancel_transfer id = transfer id ^ "/cancel"
let restart_sca id = transfer id ^ "/sca"
let swap_quote = "/swaps/quote"
let swaps = "/swaps"
let execute_swap id = "/swaps/" ^ esc id ^ "/execute"
let cards = "/cards"
let card id = "/cards/" ^ esc id
let freeze_card id = card id ^ "/freeze"
let unfreeze_card id = card id ^ "/unfreeze"
let card_limits id = card id ^ "/limits"
let pan_token id = card id ^ "/pan-token"
let card_display id = card id ^ "/display"
let card_consent id = card id ^ "/consent"
let standing_orders = "/standing-orders"
let standing_order id = "/standing-orders/" ^ esc id
let provider_sca id = "/sca/" ^ esc id
let reviews = "/ops/review"
let review_decision id = "/ops/review/" ^ esc id
let sandbox_deposit = "/sandbox/deposit"
let sandbox_kyc_decision = "/sandbox/kyc-decision"
let ws ~token = "/ws?token=" ^ esc token

let all =
  [ "POST", signup
  ; "POST", login
  ; "POST", step_up
  ; "POST", logout
  ; "GET", me
  ; "GET", kyc
  ; "POST", kyc_start
  ; "POST", kyc_complete
  ; "GET", accounts
  ; "GET", account ":id"
  ; "GET", statement ":id"
  ; "GET", beneficiaries
  ; "POST", beneficiaries
  ; "POST", trust_beneficiary ":id"
  ; "POST", fee_quote
  ; "GET", transfers
  ; "POST", transfers
  ; "GET", transfer ":id"
  ; "POST", confirm_sca ":id"
  ; "POST", cancel_transfer ":id"
  ; "POST", restart_sca ":id"
  ; "POST", swap_quote
  ; "GET", swaps
  ; "POST", execute_swap ":id"
  ; "GET", cards
  ; "POST", cards
  ; "POST", freeze_card ":id"
  ; "POST", unfreeze_card ":id"
  ; "POST", card_limits ":id"
  ; "POST", pan_token ":id"
  ; "GET", card_display ":id"
  ; "POST", card_consent ":id"
  ; "GET", standing_orders
  ; "POST", standing_orders
  ; "DELETE", standing_order ":id"
  ; "POST", provider_sca ":id"
  ; "GET", reviews
  ; "POST", review_decision ":id"
  ; "POST", sandbox_deposit
  ; "POST", sandbox_kyc_decision
  ; "GET", "/ws?token=<token>"
  ]
;;

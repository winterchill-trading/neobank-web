open! Core

type t =
  | Sign_in
  | Sign_up
  | Onboarding
  | Dashboard
  | Accounts
  | Account of string
  | Send
  | Transfer of string
  | Swap
  | Cards
  | Standing_orders
  | Statements
  | Beneficiaries
  | Review
[@@deriving compare, equal, sexp]

let of_hash hash =
  let path = String.chop_prefix_if_exists hash ~prefix:"#" in
  match String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty) with
  | [ "sign-in" ] -> Sign_in
  | [ "sign-up" ] -> Sign_up
  | [ "onboarding" ] -> Onboarding
  | [ "accounts" ] -> Accounts
  | [ "accounts"; id ] -> Account id
  | [ "send" ] -> Send
  | [ "transfers"; id ] -> Transfer id
  | [ "swap" ] -> Swap
  | [ "cards" ] -> Cards
  | [ "standing-orders" ] -> Standing_orders
  | [ "statements" ] -> Statements
  | [ "beneficiaries" ] -> Beneficiaries
  | [ "review" ] -> Review
  | _ -> Dashboard
;;

let to_hash = function
  | Sign_in -> "#/sign-in"
  | Sign_up -> "#/sign-up"
  | Onboarding -> "#/onboarding"
  | Dashboard -> "#/dashboard"
  | Accounts -> "#/accounts"
  | Account id -> "#/accounts/" ^ id
  | Send -> "#/send"
  | Transfer id -> "#/transfers/" ^ id
  | Swap -> "#/swap"
  | Cards -> "#/cards"
  | Standing_orders -> "#/standing-orders"
  | Statements -> "#/statements"
  | Beneficiaries -> "#/beneficiaries"
  | Review -> "#/review"
;;

let title = function
  | Sign_in -> "Sign in"
  | Sign_up -> "Open an account"
  | Onboarding -> "Verify your identity"
  | Dashboard -> "Home"
  | Accounts -> "Accounts"
  | Account _ -> "Account"
  | Send -> "Send"
  | Transfer _ -> "Payment"
  | Swap -> "Swap"
  | Cards -> "Cards"
  | Standing_orders -> "Standing orders"
  | Statements -> "Statements"
  | Beneficiaries -> "Payees"
  | Review -> "Review queue"
;;

let is_public = function
  | Sign_in | Sign_up -> true
  | _ -> false
;;

let menu =
  [ Dashboard; Accounts; Send; Swap; Cards; Standing_orders; Statements; Beneficiaries ]
;;

let section = function
  | Account _ -> Accounts
  | Transfer _ -> Send
  | t -> t
;;

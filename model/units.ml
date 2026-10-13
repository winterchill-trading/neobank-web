open! Core
open Neobank_api_types

type t = Z.t

let sexp_of_t z = Sexp.Atom (Z.to_string z)
let compare = Z.compare
let equal = Z.equal

let parse ~currency text =
  let exponent = (Currency.get currency).exponent in
  let text = String.strip text in
  let error msg = Or_error.error_s [%message msg (text : string) (currency : string)] in
  let int_part, frac_part =
    match String.lsplit2 text ~on:'.' with
    | None -> text, ""
    | Some (i, f) -> i, f
  in
  let int_digits =
    String.filter int_part ~f:(fun c -> not (Char.equal c ',' || Char.equal c '_'))
  in
  let all_digits s = String.for_all s ~f:Char.is_digit in
  if String.is_empty int_digits && String.is_empty frac_part
  then error "enter an amount"
  else if not (all_digits int_digits && all_digits frac_part)
  then error "not a decimal number"
  else if String.length frac_part > exponent
  then
    Or_error.error_s
      [%message
        "too many decimals" (text : string) (currency : string) ~allowed:(exponent : int)]
  else (
    let padded = frac_part ^ String.make (exponent - String.length frac_part) '0' in
    let digits = int_digits ^ padded in
    let digits = if String.is_empty digits then "0" else digits in
    Ok (Z.of_string digits))
;;

let parse_positive ~currency text =
  let%bind.Or_error z = parse ~currency text in
  if Z.leq z Z.zero
  then Or_error.error_s [%message "the amount must be above zero" (text : string)]
  else Ok z
;;

let to_string ~currency z =
  let c = Currency.get currency in
  let sign = if Z.lt z Z.zero then "-" else "" in
  let digits = Z.to_string (Z.abs z) in
  let digits =
    if String.length digits <= c.exponent
    then String.make (c.exponent + 1 - String.length digits) '0' ^ digits
    else digits
  in
  let cut = String.length digits - c.exponent in
  let int_part = String.prefix digits cut in
  let frac = String.drop_prefix digits cut in
  let frac =
    match c.kind with
    | Fiat -> frac
    | Crypto -> String.rstrip frac ~drop:(Char.equal '0')
  in
  if String.is_empty frac then sign ^ int_part else sign ^ int_part ^ "." ^ frac
;;

let of_amount ({ amount; currency } : Api_types.Amount.t) =
  let text, negative =
    match String.chop_prefix amount ~prefix:"-" with
    | Some rest -> rest, true
    | None -> amount, false
  in
  let%map.Or_error z = parse ~currency text in
  if negative then Z.neg z else z
;;

let to_amount ~currency z : Api_types.Amount.t =
  { amount = to_string ~currency z; currency }
;;

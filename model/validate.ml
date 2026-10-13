open! Core

let fail msg = Or_error.error_string msg
let compact s = String.filter s ~f:(Fn.non Char.is_whitespace)

let iban s =
  let s = compact s |> String.uppercase in
  let n = String.length s in
  if n < 15 || n > 34
  then fail "an IBAN has 15 to 34 characters"
  else if not
            (Char.is_alpha s.[0]
             && Char.is_alpha s.[1]
             && Char.is_digit s.[2]
             && Char.is_digit s.[3])
  then fail "an IBAN starts with a country code and two check digits"
  else if not (String.for_all s ~f:Char.is_alphanum)
  then fail "an IBAN has only letters and digits"
  else (
    (* Move the first four characters to the end, turn letters into 10..35 and take the
       remainder mod 97 digit by digit, so no big integers are needed. *)
    let rearranged = String.drop_prefix s 4 ^ String.prefix s 4 in
    let remainder =
      String.fold rearranged ~init:0 ~f:(fun acc c ->
        if Char.is_digit c
        then ((acc * 10) + Char.get_digit_exn c) % 97
        else (
          let v = Char.to_int c - Char.to_int 'A' + 10 in
          ((acc * 100) + v) % 97))
    in
    if remainder = 1 then Ok s else fail "the IBAN's check digits do not match")
;;

let bic s =
  let s = compact s |> String.uppercase in
  let n = String.length s in
  if (n = 8 || n = 11)
     && String.for_all (String.prefix s 6) ~f:Char.is_alpha
     && String.for_all s ~f:Char.is_alphanum
  then Ok s
  else fail "a BIC has 8 or 11 characters, the first six letters"
;;

let base58 c = Char.is_alphanum c && not (List.mem [ '0'; 'O'; 'I'; 'l' ] c ~equal:Char.equal)
let between lo hi s = String.length s >= lo && String.length s <= hi

let address ~network s =
  let s = String.strip s in
  let ok = Ok s in
  match String.uppercase network with
  | "BTC" ->
    let lower = String.lowercase s in
    if (String.is_prefix lower ~prefix:"bc1" || String.is_prefix lower ~prefix:"tb1")
       && between 14 74 s
       && String.for_all lower ~f:Char.is_alphanum
    then Ok lower
    else if (String.is_prefix s ~prefix:"1"
             || String.is_prefix s ~prefix:"3"
             || String.is_prefix s ~prefix:"m"
             || String.is_prefix s ~prefix:"n"
             || String.is_prefix s ~prefix:"2")
            && between 26 35 s
            && String.for_all s ~f:base58
    then ok
    else fail "not a Bitcoin address"
  | "ETH" | "POLYGON" | "BSC" ->
    (match String.chop_prefix s ~prefix:"0x" with
     | Some hex when String.length hex = 40 && String.for_all hex ~f:Char.is_hex_digit ->
       ok
     | _ -> fail "an address is 0x and 40 hex digits")
  | "SOL" ->
    if between 32 44 s && String.for_all s ~f:base58
    then ok
    else fail "not a Solana address"
  | "LIGHTNING" ->
    let lower = String.lowercase (String.chop_prefix_if_exists s ~prefix:"lightning:") in
    if List.exists [ "lnbc"; "lntb"; "lntbs"; "lnbcrt" ] ~f:(fun prefix ->
         String.is_prefix lower ~prefix)
       && String.length lower > 20
       && String.for_all lower ~f:Char.is_alphanum
    then Ok lower
    else fail "not a Lightning invoice (lnbc...)"
  | other -> Or_error.error_s [%message "unknown network" other]
;;

let email s =
  let s = String.strip s in
  match String.lsplit2 s ~on:'@' with
  | Some (local, domain)
    when (not (String.is_empty local))
         && String.mem domain '.'
         && (not (String.is_prefix domain ~prefix:"."))
         && (not (String.is_suffix domain ~suffix:"."))
         && not (String.exists s ~f:Char.is_whitespace) -> Ok s
  | _ -> fail "not an email address"
;;

let totp s =
  let s = compact s in
  if String.length s = 6 && String.for_all s ~f:Char.is_digit
  then Ok s
  else fail "enter the 6-digit code from your authenticator app"
;;

let password s =
  if String.length s >= 12 then Ok s else fail "use at least 12 characters"
;;

let date s =
  let s = String.strip s in
  match Date.of_string s with
  | (_ : Date.t) when String.length s = 10 -> Ok s
  | _ | (exception _) -> fail "a date is YYYY-MM-DD"
;;

let country s =
  let s = String.strip s |> String.uppercase in
  if String.length s = 2 && String.for_all s ~f:Char.is_alpha
  then Ok s
  else fail "a two-letter country code, e.g. DE"
;;

let required ~what s =
  let s = String.strip s in
  if String.is_empty s then fail (what ^ " is required") else Ok s
;;

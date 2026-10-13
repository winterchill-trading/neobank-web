open! Core
open Neobank_api_types

let group_thousands digits =
  let n = String.length digits in
  String.concat_mapi digits ~f:(fun i c ->
    if i > 0 && (n - i) % 3 = 0 then "," ^ String.of_char c else String.of_char c)
;;

let amount_text ~plus (a : Api_types.Amount.t) =
  match Units.of_amount a with
  | Error _ -> a.amount ^ " " ^ a.currency
  | Ok z ->
    let text = Units.to_string ~currency:a.currency (Z.abs z) in
    let int_part, frac =
      match String.lsplit2 text ~on:'.' with
      | Some (i, f) -> i, "." ^ f
      | None -> text, ""
    in
    let number = group_thousands int_part ^ frac in
    let sign =
      if Z.lt z Z.zero then "-" else if plus && Z.gt z Z.zero then "+" else ""
    in
    (match (Currency.get a.currency).symbol with
     | Some symbol -> sign ^ symbol ^ number
     | None -> sign ^ number ^ " " ^ a.currency)
;;

let amount a = amount_text ~plus:false a
let signed_amount a = amount_text ~plus:true a

let time s =
  match String.lsplit2 s ~on:'T' with
  | Some (date, rest) when String.length date = 10 && String.length rest >= 5 ->
    date ^ " " ^ String.prefix rest 5 ^ " UTC"
  | _ -> s
;;

let date s =
  match String.lsplit2 s ~on:'T' with
  | Some (date, _) -> date
  | None -> s
;;

let iban s =
  String.filter s ~f:(Fn.non Char.is_whitespace)
  |> String.uppercase
  |> String.to_list
  |> List.chunks_of ~length:4
  |> List.map ~f:String.of_list
  |> String.concat ~sep:" "
;;

let short ?(keep = 6) s =
  if String.length s <= keep + 7
  then s
  else String.prefix s keep ^ "..." ^ String.suffix s 4
;;

let status s =
  String.tr s ~target:'_' ~replacement:' ' |> String.lowercase |> String.capitalize
;;

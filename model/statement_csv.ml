open! Core
open Neobank_api_types

let looks_numeric s =
  (not (String.is_empty s))
  && String.for_all (String.chop_prefix_if_exists s ~prefix:"-") ~f:(fun c ->
    Char.is_digit c || Char.equal c '.')
;;

let field s =
  let s =
    match String.get s 0 with
    | ('=' | '+' | '-' | '@') when not (looks_numeric s) -> "'" ^ s
    | _ | (exception _) -> s
  in
  if String.exists s ~f:(fun c ->
       Char.equal c ',' || Char.equal c '"' || Char.equal c '\n' || Char.equal c '\r')
  then "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
  else s
;;

let to_string (s : Api_types.Statement.t) =
  let line fields = String.concat ~sep:"," (List.map fields ~f:field) ^ "\r\n" in
  line [ "at"; "amount"; "currency"; "code"; "reference"; "balance" ]
  ^ String.concat
      (List.map s.lines ~f:(fun (l : Api_types.Statement.Line.t) ->
         line
           [ l.at
           ; l.amount.amount
           ; l.amount.currency
           ; l.code
           ; l.reference
           ; l.balance.amount
           ]))
;;

let file_name (s : Api_types.Statement.t) =
  let safe x = String.map x ~f:(fun c -> if Char.is_alphanum c then c else '-') in
  sprintf
    "statement-%s-%s-%s.csv"
    (safe s.account_id)
    (Format.date s.from)
    (Format.date s.until)
;;

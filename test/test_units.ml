open! Core
open Neobank_web_model

let show currency text =
  match Units.parse ~currency text with
  | Ok z -> print_endline (Z.to_string z ^ " -> " ^ Units.to_string ~currency z)
  | Error e -> print_s [%sexp (e : Error.t)]
;;

let%expect_test "parse and print" =
  show "EUR" "12.3";
  show "EUR" "1,234.56";
  show "EUR" " 7 ";
  show "EUR" ".5";
  show "EUR" "1.234";
  show "EUR" "-1";
  show "EUR" "1e3";
  show "EUR" "";
  show "BTC" "0.00015000";
  show "BTC" "2";
  show "ETH" "1.000000000000000001";
  show "ETH" "12345.5";
  [%expect
    {|
    1230 -> 12.30
    123456 -> 1234.56
    700 -> 7.00
    50 -> 0.50
    ("too many decimals" (text 1.234) (currency EUR) (allowed 2))
    ("not a decimal number" (text -1) (currency EUR))
    ("not a decimal number" (text 1e3) (currency EUR))
    ("enter an amount" (text "") (currency EUR))
    15000 -> 0.00015
    200000000 -> 2
    1000000000000000001 -> 1.000000000000000001
    12345500000000000000000 -> 12345.5
    |}]
;;

let%expect_test "signed amounts from the API" =
  List.iter
    [ { Neobank_api_types.Api_types.Amount.amount = "-12.50"; currency = "EUR" }
    ; { amount = "0"; currency = "BTC" }
    ]
    ~f:(fun a ->
      let z = Units.of_amount a |> Or_error.ok_exn in
      print_endline (Units.to_string ~currency:a.currency z));
  [%expect
    {|
    -12.50
    0
    |}]
;;

let%test_unit "to_string then parse is the identity" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       (Quickcheck.Generator.of_list (List.map Currency.all ~f:(fun c -> c.code)))
       (Int.gen_incl 0 Int.max_value))
    ~f:(fun (currency, n) ->
      let z = Z.mul (Z.of_int n) (Z.of_int 1_000) in
      [%test_result: Units.t]
        ~expect:z
        (Units.parse ~currency (Units.to_string ~currency z) |> Or_error.ok_exn))
;;

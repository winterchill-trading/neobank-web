open! Core
open Neobank_web_model

let amount amount currency =
  print_endline
    (Format.amount { amount; currency }
     ^ "    "
     ^ Format.signed_amount { amount; currency })
;;

let%expect_test "amounts" =
  amount "1234567.5" "EUR";
  amount "-12.5" "EUR";
  amount "0.00015000" "BTC";
  amount "1500.25" "USDC";
  amount "0" "EUR";
  amount "lots" "EUR";
  [%expect
    {|
    €1,234,567.50    +€1,234,567.50
    -€12.50    -€12.50
    0.00015 BTC    +0.00015 BTC
    1,500.25 USDC    +1,500.25 USDC
    €0.00    €0.00
    lots EUR    lots EUR
    |}]
;;

let%expect_test "times, IBANs, short text" =
  print_endline (Format.time "2026-10-14T08:30:12.123Z");
  print_endline (Format.time "yesterday");
  print_endline (Format.date "2026-10-14T08:30:12Z");
  print_endline (Format.iban "de89370400440532013000");
  print_endline (Format.short "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh");
  print_endline (Format.short "short");
  print_endline (Format.status "pending_review");
  [%expect
    {|
    2026-10-14 08:30 UTC
    yesterday
    2026-10-14
    DE89 3704 0044 0532 0130 00
    bc1qxy...0wlh
    short
    Pending review
    |}]
;;

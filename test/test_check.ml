open! Core
open Neobank_web_model

let show = function
  | Ok s -> print_endline ("ok " ^ s)
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let%expect_test "IBAN" =
  List.iter
    ~f:(fun s -> show (Check.iban s))
    [ "DE89 3704 0044 0532 0130 00"
    ; "de89370400440532013000"
    ; "GB82 WEST 1234 5698 7654 32"
    ; "DE88 3704 0044 0532 0130 00"
    ; "DE89"
    ; "1234 5678 9012 3456"
    ];
  [%expect
    {|
    ok DE89370400440532013000
    ok DE89370400440532013000
    ok GB82WEST12345698765432
    error: the IBAN's check digits do not match
    error: an IBAN has 15 to 34 characters
    error: an IBAN starts with a country code and two check digits
    |}]
;;

(* Changing any single digit of a valid IBAN breaks mod 97: 10^k mod 97 is never 0, so the
   remainder moves by a non-zero multiple. *)
let%test_unit "a single changed digit is caught" =
  let valid = "DE89370400440532013000" in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 (Int.gen_incl 4 21) (Int.gen_incl 1 9))
    ~f:(fun (pos, delta) ->
      let d = Char.get_digit_exn valid.[pos] in
      let changed =
        String.mapi valid ~f:(fun i c ->
          if i = pos then Char.of_int_exn (Char.to_int '0' + ((d + delta) % 10)) else c)
      in
      [%test_pred: string Or_error.t] Result.is_error (Check.iban changed))
;;

let%expect_test "BIC, email, codes, dates" =
  show (Check.bic "deutdeff");
  show (Check.bic "DEUTDEFF500");
  show (Check.bic "DEU1");
  show (Check.email " demo@example.com ");
  show (Check.email "demo@example");
  show (Check.totp "123 456");
  show (Check.totp "12345");
  show (Check.password "short");
  show (Check.date "1990-02-28");
  show (Check.date "1990-02-30");
  show (Check.country "de");
  show (Check.required ~what:"Name" "  ");
  [%expect
    {|
    ok DEUTDEFF
    ok DEUTDEFF500
    error: a BIC has 8 or 11 characters, the first six letters
    ok demo@example.com
    error: not an email address
    ok 123456
    error: enter the 6-digit code from your authenticator app
    error: use at least 12 characters
    ok 1990-02-28
    error: a date is YYYY-MM-DD
    ok DE
    error: Name is required
    |}]
;;

let%expect_test "addresses" =
  List.iter
    ~f:(fun (network, s) -> show (Check.address ~network s))
    [ "BTC", "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; "BTC", "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"
    ; "BTC", "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
    ; "ETH", "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
    ; "ETH", "0x1234"
    ; ( "LIGHTNING"
      , "lightning:LNTB10U1PJ9X7ZPP5QQQSYQCYQ5RQWZQFQQQSYQCYQ5RQWZQFQQQSYQCYQ5RQWZQFQYPQ"
      )
    ; "LIGHTNING", "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; "SOL", "4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T"
    ; "DOGE", "D"
    ];
  [%expect
    {|
    ok bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh
    ok 1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2
    error: not a Bitcoin address
    ok 0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe
    error: an address is 0x and 40 hex digits
    ok lntb10u1pj9x7zpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq
    error: not a Lightning invoice (lnbc...)
    ok 4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T
    error: ("unknown network" DOGE)
    |}]
;;

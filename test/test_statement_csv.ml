open! Core
open Neobank_api_types
open Neobank_web_model

let eur amount : Api_types.Amount.t = { amount; currency = "EUR" }

let%expect_test "quoting and formula guard" =
  let statement : Api_types.Statement.t =
    { account_id = "acc_eur"
    ; currency = "EUR"
    ; from = "2026-09-01T00:00:00Z"
    ; until = "2026-09-30T23:59:59Z"
    ; opening = eur "100.00"
    ; closing = eur "60.00"
    ; lines =
        [ { at = "2026-09-02T10:00:00Z"
          ; amount = eur "-25.00"
          ; code = "sepa_out"
          ; reference = "Rent, September"
          ; balance = eur "75.00"
          }
        ; { at = "2026-09-03T10:00:00Z"
          ; amount = eur "-15.00"
          ; code = "card"
          ; reference = "=HYPERLINK(\"x\")"
          ; balance = eur "60.00"
          }
        ]
    }
  in
  print_string
    (String.substr_replace_all
       (Statement_csv.to_string statement)
       ~pattern:"\r\n"
       ~with_:"\n");
  print_endline (Statement_csv.file_name statement);
  [%expect
    {|
    at,amount,currency,code,reference,balance
    2026-09-02T10:00:00Z,-25.00,EUR,sepa_out,"Rent, September",75.00
    2026-09-03T10:00:00Z,-15.00,EUR,card,"'=HYPERLINK(""x"")",60.00
    statement-acc-eur-2026-09-01-2026-09-30.csv
    |}]
;;

open! Core
open Neobank_web_model

let%expect_test "ids and dates are escaped" =
  print_endline (Endpoint.account "a/b c");
  print_endline (Endpoint.statement "acc_1" ~from:"2026-09-01" ~until:"2026-09-30");
  print_endline (Endpoint.statement "acc_1");
  print_endline (Endpoint.ws ~token:"t+k/=");
  [%expect
    {|
    /accounts/a%2Fb%20c
    /accounts/acc_1/statement?from=2026-09-01&until=2026-09-30
    /accounts/acc_1/statement
    /ws?token=t%2Bk%2F%3D
    |}]
;;

let%expect_test "no route is listed twice" =
  let dups = List.find_all_dups Endpoint.all ~compare:[%compare: string * string] in
  print_s [%sexp (dups : (string * string) list)];
  [%expect {| () |}]
;;

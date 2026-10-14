open! Core
open Neobank_web_model

let%expect_test "card display settings decode and never print the UI secret" =
  let t =
    Hosted.Card_display.t_of_yojson
      (Yojson.Safe.from_string
         {|{"provider":"striga","application_id":"app-1","ui_secret":"not-a-real-secret","user_id":"u-1","card_id":"c-1","other":true}|})
  in
  print_s [%sexp (t : Hosted.Card_display.t)];
  print_endline (Hosted.Card_display.script t);
  [%expect
    {|
    ((provider striga) (script_url ()) (application_id app-1)
     (ui_secret <hidden>) (user_id u-1) (card_id c-1))
    https://www.vault.striga.eu/web/sandbox/v1.1/client.min.js
    |}]
;;

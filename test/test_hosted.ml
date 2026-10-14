open! Core
open Neobank_api_types
open Neobank_web_model

let show json =
  let d = Api_types.Card.Display.t_of_yojson (Yojson.Safe.from_string json) in
  print_s
    [%sexp
      (Hosted.Card_display.is_simulated d : bool)
      , (Option.map (Hosted.Card_display.plugin d) ~f:(fun p ->
           p.application_id, p.user_id)
         : (string * string) option)];
  print_endline (Hosted.Card_display.script d)
;;

let%expect_test "card display settings from the API" =
  show
    {|{"provider":"striga","application_id":"app-1","ui_secret":"not-a-real-secret","user_id":"u-1","card_id":"c-1","other":true}|};
  show {|{"provider":"fake","card_id":"card_1"}|};
  show {|{"provider":"x","card_id":"c","script_url":"https://example.com/p.js"}|};
  [%expect
    {|
    (false ((app-1 u-1)))
    https://www.vault.striga.eu/web/sandbox/v1.1/client.min.js
    (false ())
    https://www.vault.striga.eu/web/sandbox/v1.1/client.min.js
    (false ())
    https://example.com/p.js
    |}]
;;

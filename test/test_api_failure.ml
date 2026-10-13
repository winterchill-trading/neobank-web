open! Core
open Neobank_web_model

let show status body = print_s [%sexp (Api_failure.of_response ~status body : Api_failure.t)]

let%expect_test "answers the API refuses with" =
  show 422 {|{"error":"insufficient_funds","message":"not enough money","extra":1}|};
  show
    403
    {|{"error":"sca_required","message":"confirm it is you","challenge":{"challenge_id":"c1","factors":["knowledge","possession"],"expires_at":"2026-10-14T08:35:00.000Z","attempts_left":5}}|};
  show 502 "<html>Bad Gateway</html>";
  show 500 "";
  [%expect
    {|
    ((status 422) (error insufficient_funds) (message "not enough money")
     (challenge ()))
    ((status 403) (error sca_required) (message "confirm it is you")
     (challenge
      (((challenge_id c1) (factors (knowledge possession))
        (expires_at 2026-10-14T08:35:00.000Z) (attempts_left 5)))))
    ((status 502) (error http_error) (message "<html>Bad Gateway</html>")
     (challenge ()))
    ((status 500) (error http_error) (message "HTTP 500") (challenge ()))
    |}]
;;

let%expect_test "clock text" =
  print_endline (Clock.iso 1_760_430_612_123.);
  print_endline (Clock.date 1_760_430_612_123.);
  [%expect
    {|
    2025-10-14T08:30:12.123Z
    2025-10-14
    |}]
;;

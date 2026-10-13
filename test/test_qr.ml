open! Core
open Neobank_web_model

let cases : (string * Qr.Ecc.t * int option) list =
  [ "HELLO", M, None
  ; "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh", M, None
  ; "bitcoin:bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh?amount=0.001", L, Some 3
  ; "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe", Q, None
  ; "DE89370400440532013000", H, None
  ; "lntb10u1" ^ String.make 250 'q', M, None
  ; String.make 1000 'x', L, None
  ]
;;

(* Version, mask and MD5 of the module rows match the qrcodegen Python package 1.8
   (`QrCode.encode_segments([QrSegment.make_bytes(...)], ecc, 1, 40, mask, False)`) for
   every case, masks chosen by penalty included, and OpenCV's QRCodeDetector reads the
   symbols back. segno 1.6 draws different modules for the same input, so it is not the
   reference here. *)
let%expect_test "matches a reference encoder" =
  List.iter cases ~f:(fun (text, ecc, mask) ->
    let qr = Qr.encode ~ecc ?mask text |> Or_error.ok_exn in
    printf
      "%d %d %s\n"
      (Qr.version qr)
      (Qr.mask qr)
      (Md5.to_hex (Md5.digest_string (Qr.to_text qr))));
  [%expect
    {|
    1 4 25eb820f38149690c354cef796b612d6
    3 6 27cefa8fe97b561f97b51375055e8e11
    4 3 9ba080d48157c5843c2bc023cb15a8f2
    4 4 00edc9b374bed8afa7af8e81d2cd00c1
    3 3 4356489ead638bffd42d2c33efcf4b50
    12 1 55147a2b417fbff205b8cf71e629773e
    22 0 6b026e9628f462d0cbed27106d437fcf
    |}]
;;

let%expect_test "a small symbol" =
  let qr = Qr.encode "HELLO" |> Or_error.ok_exn in
  print_endline (Qr.to_text qr);
  print_endline (String.prefix (Qr.svg_path qr) 40);
  [%expect
    {|
    #######.##.#..#######
    #.....#..##.#.#.....#
    #.###.#..####.#.###.#
    #.###.#.#..#..#.###.#
    #.###.#.#...#.#.###.#
    #.....#.#.##..#.....#
    #######.#.#.#.#######
    ........#####........
    #...#.######.#####..#
    ...###..#.###..#.####
    #.##..#.#.##..###..#.
    ###..#...#...##.#....
    ..#.###..#..###...##.
    ........###.###..#.##
    #######.##..##...#.#.
    #.....#....##..#...#.
    #.###.#.#..#..###.#.#
    #.###.#....##....#.##
    #.###.#..###..####...
    #.....#..#...##......
    #######.#...#####.#.#
    M0 0h7v1h-7zM8 0h2v1h-2zM11 0h1v1h-1zM14
    |}]
;;

let%expect_test "too long and bad masks" =
  print_s
    [%sexp
      (Qr.encode ~ecc:H (String.make 1300 'x') |> Or_error.map ~f:Qr.version
       : int Or_error.t)];
  print_s [%sexp (Qr.encode ~mask:8 "x" |> Or_error.map ~f:Qr.version : int Or_error.t)];
  [%expect
    {|
    (Error ("too long for a QR code" (len 1300) (ecc H)))
    (Error ("mask must be 0 to 7" (mask (8))))
    |}]
;;

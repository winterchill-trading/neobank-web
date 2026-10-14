open! Core
open! Bonsai_web
open Js_of_ocaml
module Route = Neobank_web_model.Route

let current () = Route.of_hash (Js.to_string Dom_html.window##.location##.hash)

let navigate route =
  Effect.of_thunk (fun () ->
    Dom_html.window##.location##.hash := Js.string (Route.to_hash route))
;;

let on_change f =
  Dom_html.window##.onhashchange
  := Dom_html.handler (fun _ ->
    f (current ());
    Js._true)
;;

let query () =
  let search = Js.to_string Dom_html.window##.location##.search in
  String.chop_prefix_if_exists search ~prefix:"?"
  |> String.split ~on:'&'
  |> List.filter_map ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some (k, v) -> Some (Url.urldecode k, Url.urldecode v)
    | None when String.is_empty pair -> None
    | None -> Some (Url.urldecode pair, ""))
;;

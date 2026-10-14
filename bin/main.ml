open! Core
open! Bonsai_web
open Neobank_web_ui

let () =
  let route = Bonsai.Expert.Var.create (Nav.current ()) in
  Nav.on_change (fun r -> Bonsai.Expert.Var.set route r);
  let backend = Backend.of_query (Nav.query ()) in
  Start.start (App.component ~backend ~route:(Bonsai.Expert.Var.value route))
;;

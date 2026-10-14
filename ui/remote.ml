open! Core
open! Async_kernel
open! Bonsai_web

type 'a t =
  | Idle
  | Loading
  | Loaded of 'a
  | Failed of Error.t

let fetch ~equal ~load key (local_ graph) =
  let open Bonsai.Let_syntax in
  let answer, set_answer = Bonsai.state_opt graph in
  let callback =
    let%map set_answer in
    function
    | None -> Effect.Ignore
    | Some k ->
      let%bind.Effect result = Effect.of_deferred_fun load k in
      set_answer (Some (k, result))
  in
  Bonsai.Edge.on_change ~equal:(Option.equal equal) key ~callback graph;
  let%map key and answer in
  match key, answer with
  | None, _ -> Idle
  | Some _, None -> Loading
  | Some k, Some (k', result) ->
    if not (equal k k')
    then Loading
    else (
      match result with
      | Ok a -> Loaded a
      | Error e -> Failed e)
;;

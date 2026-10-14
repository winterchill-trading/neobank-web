open! Core
open! Bonsai_web
module N = Vdom.Node
module A = Vdom.Attr

type t = string String.Map.t [@@deriving sexp, equal]

let get t name = Map.find t name |> Option.value ~default:""

(* A page builds the new form from the one it rendered with, and two input events can
   arrive before the next render (fast typing, or a test filling several fields). So the
   setter sends what changed against the rendered form, and the change is applied to the
   latest state instead of replacing it. *)
let state ?(init = []) (local_ graph) =
  let open Bonsai.Let_syntax in
  let model, inject =
    Bonsai.state_machine
      ~equal
      ~default_model:(String.Map.of_alist_reduce init ~f:(fun _ b -> b))
      ~apply_action:(fun _ latest (base, next) ->
        Map.symmetric_diff base next ~data_equal:String.equal
        |> Sequence.fold ~init:latest ~f:(fun acc (key, change) ->
          match change with
          | `Left _ -> Map.remove acc key
          | `Right data | `Unequal (_, data) -> Map.set acc ~key ~data))
      graph
  in
  let set =
    let%map model and inject in
    fun next -> inject (model, next)
  in
  model, set
;;

let input ?(type_ = "text") ?placeholder ?error ?hint ~form ~set ~name ~label () =
  N.label
    ~attrs:[ A.class_ "field" ]
    [ N.span [ N.text label ]
    ; N.input
        ~attrs:
          (List.filter_opt
             [ Some (A.type_ type_)
             ; Some (A.name name)
             ; Some (A.value (get form name))
             ; Some (A.on_input (fun _ v -> set (Map.set form ~key:name ~data:v)))
             ; Option.map placeholder ~f:A.placeholder
             ; Option.map error ~f:(fun _ -> A.class_ "invalid")
             ])
        ()
    ; (match error, hint with
       | Some msg, _ -> N.span ~attrs:[ A.class_ "field-error" ] [ N.text msg ]
       | None, Some hint -> N.span ~attrs:[ A.class_ "field-hint" ] [ N.text hint ]
       | None, None -> N.none)
    ]
;;

let select ~form ~set ~name ~label options =
  let current = get form name in
  N.label
    ~attrs:[ A.class_ "field" ]
    [ N.span [ N.text label ]
    ; N.select
        ~attrs:
          [ A.name name; A.on_change (fun _ v -> set (Map.set form ~key:name ~data:v)) ]
        (List.map options ~f:(fun (value, text) ->
           N.option
             ~attrs:(A.value value :: (if String.equal value current then [ A.selected ] else []))
             [ N.text text ]))
    ]
;;

let checked form name = String.equal (get form name) "true"

let checkbox ~form ~set ~name ~label =
  let on = checked form name in
  N.label
    ~attrs:[ A.class_ "check" ]
    [ N.input
        ~attrs:
          [ A.type_ "checkbox"
          ; A.name name
          ; A.checked_prop on
          ; A.on_click (fun _ ->
              set (Map.set form ~key:name ~data:(if on then "false" else "true")))
          ]
        ()
    ; N.text (" " ^ label)
    ]
;;

let error_of = function
  | Ok _ -> None
  | Error e -> Some (Error.to_string_hum e)
;;

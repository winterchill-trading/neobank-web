open! Core
open! Bonsai_web
open Neobank_api_types
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr

let link ?(attrs = []) route children =
  N.a ~attrs:(A.href (Route.to_hash route) :: attrs) children
;;

let button ?id ?(primary = false) ?(danger = false) ?(disabled = false) ~on_click label =
  N.button
    ~attrs:
      (List.filter_opt
         [ Option.map id ~f:A.id
         ; Some
             (A.classes
                ("btn"
                 :: List.filter_opt
                      [ Option.some_if primary "primary"; Option.some_if danger "danger" ]))
         ; Some (A.disabled' disabled)
         ; Some (A.on_click (fun _ -> on_click))
         ])
    [ N.text label ]
;;

let failure (f : Api_failure.t) =
  N.div ~attrs:[ A.class_ "error" ] [ N.text (Api_failure.to_string f) ]
;;

let notice ?(kind = "notice") text = N.div ~attrs:[ A.class_ kind ] [ N.text text ]
let loading = N.div ~attrs:[ A.class_ "loading" ] [ N.text "Loading..." ]

let remote (r : _ Remote.t) ~f =
  match r with
  | Idle -> N.none
  | Loading -> loading
  | Failed e -> failure e
  | Loaded a -> f a
;;

let section ?id title children =
  N.section
    ~attrs:(A.class_ "card" :: Option.to_list (Option.map id ~f:A.id))
    (N.h2 [ N.text title ] :: children)
;;

let stat ~label value =
  N.div
    ~attrs:[ A.class_ "stat" ]
    [ N.div ~attrs:[ A.class_ "stat-label" ] [ N.text label ]
    ; N.div ~attrs:[ A.class_ "stat-value" ] [ N.text value ]
    ]
;;

let stats children = N.div ~attrs:[ A.class_ "stats" ] children

let table ?id ~header ~numeric rows =
  let cell_attrs i = if List.mem numeric i ~equal:Int.equal then [ A.class_ "num" ] else [] in
  N.table
    ~attrs:(A.class_ "grid" :: Option.to_list (Option.map id ~f:A.id))
    [ N.thead
        [ N.tr (List.mapi header ~f:(fun i h -> N.th ~attrs:(cell_attrs i) [ N.text h ])) ]
    ; N.tbody
        (List.map rows ~f:(fun cells ->
           N.tr (List.mapi cells ~f:(fun i c -> N.td ~attrs:(cell_attrs i) [ c ]))))
    ]
;;

let amount a = N.span ~attrs:[ A.class_ "amount" ] [ N.text (Format.amount a) ]

let movement ~direction (a : Api_types.Amount.t) =
  match direction with
  | "in" -> N.span ~attrs:[ A.class_ "pos" ] [ N.text ("+" ^ Format.amount a) ]
  | _ -> N.span ~attrs:[ A.class_ "neg" ] [ N.text ("-" ^ Format.amount a) ]
;;

let badge status =
  N.span ~attrs:[ A.classes [ "badge"; status ] ] [ N.text (Format.status status) ]
;;

let detail ~label value =
  N.div
    ~attrs:[ A.class_ "detail" ]
    [ N.span ~attrs:[ A.class_ "detail-label" ] [ N.text label ]
    ; N.span ~attrs:[ A.classes [ "mono"; "selectable" ] ] [ N.text value ]
    ]
;;

let account_label (a : Api_types.Account.t) =
  sprintf "%s account" (Currency.get a.currency).name
;;

let account_select ~name ~label ~accounts ~value ~on_change =
  N.label
    ~attrs:[ A.class_ "field" ]
    [ N.span [ N.text label ]
    ; N.select
        ~attrs:[ A.name name; A.on_change (fun _ v -> on_change v) ]
        (N.option ~attrs:[ A.value ""; A.disabled ] [ N.text "Choose an account" ]
         :: List.map accounts ~f:(fun (a : Api_types.Account.t) ->
           let balance =
             Option.value_map a.balance ~default:"" ~f:(fun b ->
               " - " ^ Format.amount b.available)
           in
           N.option
             ~attrs:
               (A.value a.id :: (if String.equal a.id value then [ A.selected ] else []))
             [ N.text (account_label a ^ balance) ]))
    ]
;;

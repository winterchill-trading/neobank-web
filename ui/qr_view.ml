open! Core
open! Bonsai_web
open Neobank_web_model
module N = Vdom.Node
module A = Vdom.Attr
module Svg = Virtual_dom_svg

let view ?label text =
  match Qr.encode ~ecc:M text with
  | Error _ -> Widgets.notice "Too long to show as a QR code."
  | Ok qr ->
    let size = Qr.size qr + 8 in
    N.figure
      ~attrs:[ A.class_ "qr" ]
      [ Svg.Node.svg
          ~attrs:
            [ Svg.Attr.viewbox
                ~min_x:0.
                ~min_y:0.
                ~width:(Float.of_int size)
                ~height:(Float.of_int size)
            ; A.class_ "qr-code"
            ; A.create "shape-rendering" "crispEdges"
            ; A.create "role" "img"
            ; A.create "aria-label" (Option.value label ~default:text)
            ]
          [ Svg.Node.rect
              ~attrs:
                [ Svg.Attr.width (Float.of_int size)
                ; Svg.Attr.height (Float.of_int size)
                ; A.class_ "qr-bg"
                ]
              []
          ; Svg.Node.path
              ~attrs:
                [ A.create "d" (Qr.svg_path qr)
                ; A.create "transform" "translate(4 4)"
                ; A.class_ "qr-fg"
                ]
              []
          ]
      ; (match label with
         | None -> N.none
         | Some l -> N.figcaption [ N.text l ])
      ]
;;

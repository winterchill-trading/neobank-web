open! Core

(* https://docs.sumsub.com/docs/get-started-with-web-sdk ("use the standalone package") *)
let kyc_sdk_script = "https://static.sumsub.com/idensic/static/sns-websdk-builder.js"

(* https://docs.striga.com/reference/methods ("Striga JS client SDK") *)
let card_plugin_sandbox_script =
  "https://www.vault.striga.eu/web/sandbox/v1.1/client.min.js"
;;

module Card_display = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type t =
    { provider : string
    ; script_url : string option [@yojson.option]
    ; application_id : string
    ; ui_secret : string
    ; user_id : string
    ; card_id : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let sexp_of_t t =
    [%sexp
      { provider : string = t.provider
      ; script_url : string option = t.script_url
      ; application_id : string = t.application_id
      ; ui_secret = "<hidden>"
      ; user_id : string = t.user_id
      ; card_id : string = t.card_id
      }]
  ;;

  let script t = Option.value t.script_url ~default:card_plugin_sandbox_script
end

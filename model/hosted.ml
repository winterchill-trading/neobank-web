open! Core

(* https://docs.sumsub.com/docs/get-started-with-web-sdk ("use the standalone package") *)
let kyc_sdk_script = "https://static.sumsub.com/idensic/static/sns-websdk-builder.js"

(* https://docs.striga.com/reference/methods ("Striga JS client SDK") *)
let card_plugin_sandbox_script =
  "https://www.vault.striga.eu/web/sandbox/v1.1/client.min.js"
;;

module Card_display = struct
  type t = Neobank_api_types.Api_types.Card.Display.t

  type plugin =
    { ui_secret : string
    ; application_id : string
    ; user_id : string
    }

  let plugin (d : t) =
    match d.ui_secret, d.application_id, d.user_id with
    | Some ui_secret, Some application_id, Some user_id ->
      Some { ui_secret; application_id; user_id }
    | _ -> None
  ;;

  let is_simulated (d : t) = String.equal d.provider "simulated"
  let script (d : t) = Option.value d.script_url ~default:card_plugin_sandbox_script
end

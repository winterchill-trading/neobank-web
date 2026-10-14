(** Settings for the provider components embedded in two pages, and the documented URLs
    of their scripts. The scripts are loaded only on those pages (onboarding and cards),
    never at startup.

    - Identity checks: the provider runs them in a KYC vendor's Web SDK (WebSDK 2.0); the
      API's [POST /kyc/start] returns the SDK access token for a hosted step.
    - Card number, CVV and PIN: the provider's card display plugin draws them in its own
      iframe, so they never pass through this app or the API. It needs the provider's
      application id and a UI secret meant for the browser; both come from the API at
      runtime ({!Card_display}), never from this repository. *)

open! Core

(** The KYC vendor's standalone WebSDK builder, which defines [window.snsWebSdk]. *)
val kyc_sdk_script : string

(** The card display plugin's sandbox build, which defines [window.StrigaUXPlugin]. Used
    when the API does not name a script itself. *)
val card_plugin_sandbox_script : string

module Card_display : sig
  (** What the card page needs to start the plugin for one card: [GET /cards/:id/display].
      [user_id] and [card_id] are the provider's ids, not ours. [ui_secret] is printed as
      ["<hidden>"] in sexps. *)
  type t =
    { provider : string (** ["striga"], or ["simulated"] in mock mode *)
    ; script_url : string option
    ; application_id : string
    ; ui_secret : string
    ; user_id : string
    ; card_id : string
    }
  [@@deriving sexp_of, yojson]

  val script : t -> string
end

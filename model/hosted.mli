(** Settings for the provider components embedded in two pages, and the documented URLs of
    their scripts. The scripts are loaded only on those pages (onboarding and cards),
    never at startup.

    - Identity checks: the provider runs them in a KYC vendor's Web SDK (WebSDK 2.0); the
      API's [POST /kyc/start] returns the SDK access token for a hosted step.
    - Card number, CVV and PIN: the provider's card display plugin draws them in its own
      iframe, so they never pass through this app or the API. It needs the provider's
      application id and a UI secret meant for the browser; both come from the API at
      runtime ([GET /cards/:id/display]), never from this repository. *)

open! Core

(** The KYC vendor's standalone WebSDK builder, which defines [window.snsWebSdk]. *)
val kyc_sdk_script : string

(** The card display plugin's sandbox build, which defines [window.StrigaUXPlugin]. Used
    when the API does not name a script itself. *)
val card_plugin_sandbox_script : string

module Card_display : sig
  type t = Neobank_api_types.Api_types.Card.Display.t

  (** What the plugin's [create] and [requestConsent] need: present only for a provider
      with a card component ([fake] has none; the mock answers ["simulated"]). *)
  type plugin =
    { ui_secret : string
    ; application_id : string
    ; user_id : string
    }

  val plugin : t -> plugin option
  val is_simulated : t -> bool

  (** The API's script, else the sandbox build above. *)
  val script : t -> string
end

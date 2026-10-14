(** Glue to the two provider components that run their own iframes: the KYC vendor's
    WebSDK on the onboarding page and the card display plugin on the cards page. Their
    scripts are loaded on first use (see {!Neobank_web_model.Hosted}), and what they draw
    goes into a {!host} element that the virtual DOM leaves alone. *)

open! Core
open! Async_kernel
open! Bonsai_web

(** An empty [<div id=dom_id>] owned by the page, not by Bonsai's diffing, so the iframe a
    component adds survives re-renders. *)
val host : ?class_:string -> string -> Vdom.Node.t

module Kyc_sdk : sig
  (** Loads the WebSDK and launches it into [#container] with the access token. [refresh]
      gets a new token when the SDK reports the old one expired. [on_message] receives the
      SDK's event names ([idCheck.onStepCompleted], [idCheck.onError], ...). *)
  val launch
    :  container:string
    -> token:string
    -> refresh:(unit -> string Or_error.t Deferred.t)
    -> on_message:(string -> unit)
    -> unit Or_error.t Deferred.t
end

module Card_plugin : sig
  (** Loads the plugin script and calls [create(uiSecret, {applicationId})]. *)
  val create : Neobank_web_model.Hosted.Card_display.t -> unit Or_error.t Deferred.t

  (** [requestConsent({userId})]: the provider sends the customer a code by SMS and email.
      Returns the challenge id. *)
  val request_consent : user_id:string -> string Or_error.t Deferred.t

  (** [render(key, {id, cardId, authToken, maskedCardNumber}, callback)] for [key] =
      [cardNumber], [cvv] or [pin]. *)
  val render
    :  key:string
    -> dom_id:string
    -> card_id:string
    -> auth_token:string
    -> masked:string option
    -> unit Or_error.t Deferred.t
end

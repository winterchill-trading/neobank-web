(** Virtual cards: issue, freeze and unfreeze, limits, and the card details.

    The full number, CVV and PIN never reach this app or the API: the provider's card
    display plugin draws them in its own iframes. The flow is the provider's: the plugin
    asks for consent (the provider sends the customer a code), the API confirms the code
    and hands out a short-lived token, and the plugin renders with it. With the fake
    provider the API has no plugin settings, so the page stops after the token; in mock
    mode a stand-in shows test digits. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

(** Identity verification. Code steps (email, mobile) take the code we send; document and
    liveness steps run in the KYC vendor's WebSDK, started with the token from
    [POST /kyc/start] (in mock mode a stand-in flow takes its place). The page reloads the
    status every few seconds while a step is open, since the provider reports results to
    the API, not to the browser. *)

open! Core
open! Bonsai_web

val component : Page.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t

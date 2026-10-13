(** Paths of the neobank API, relative to its root. One place to change when a route
    moves. Ids are URL-escaped. *)

open! Core

val signup : string
val login : string
val step_up : string
val logout : string
val me : string
val kyc : string
val kyc_start : string
val kyc_complete : string
val accounts : string
val account : string -> string

(** [from] and [until] are [YYYY-MM-DD]. *)
val statement : ?from:string -> ?until:string -> string -> string

val beneficiaries : string
val trust_beneficiary : string -> string
val fee_quote : string
val transfers : string
val transfer : string -> string
val confirm_sca : string -> string
val cancel_transfer : string -> string
val restart_sca : string -> string
val swap_quote : string
val swaps : string
val execute_swap : string -> string
val cards : string
val card : string -> string
val freeze_card : string -> string
val unfreeze_card : string -> string
val card_limits : string -> string
val pan_token : string -> string

(** Settings for the provider's card display component (script, application id, UI secret,
    the provider's ids of user and card). *)
val card_display : string -> string

(** Tells the API the challenge the card display component started. *)
val card_consent : string -> string

val standing_orders : string
val standing_order : string -> string

(** A provider's own challenge (a code it sent by SMS or email). *)
val provider_sca : string -> string

val reviews : string
val review_decision : string -> string
val sandbox_deposit : string

(** The websocket for pushes; the token travels in the query string because browsers
    cannot set headers on a websocket. *)
val ws : token:string -> string

(** Every path pattern, with [:id] for ids, for the README table and the smoke test. *)
val all : (string * string) list

(** Where the app's data comes from: the neobank API over HTTP, or
    {!Neobank_web_model.Mock_bank} in memory so every page works without a server
    ([?mock=1]). *)

open! Core
open! Async_kernel
open Neobank_api_types
open Neobank_web_model
module A := Api_types

type 'a res = ('a, Api_failure.t) Result.t Deferred.t

type t =
  { description : string (** shown in the header: ["mock data"] or the API URL *)
  ; is_mock : bool
  ; signup : A.Auth.Signup_request.t -> A.Auth.Signup_response.t res
  ; login : A.Auth.Login_request.t -> A.Auth.Session.t res
  ; step_up : token:string -> A.Auth.Step_up_request.t -> A.Auth.Session.t res
  ; logout : token:string -> unit res
  ; me : token:string -> A.Me.t res
  ; kyc_status : token:string -> A.Kyc.Status.t res
  ; kyc_start : token:string -> A.Kyc.Start_request.t -> A.Kyc.Session.t res
  ; kyc_complete : token:string -> A.Kyc.Complete_request.t -> A.Kyc.Status.t res
  ; accounts : token:string -> A.Account.t list res
  ; account : token:string -> string -> A.Account.t res
  ; statement :
      token:string -> ?from:string -> ?until:string -> string -> A.Statement.t res
  ; deposit : token:string -> A.Sandbox.Deposit.t -> unit res
  ; beneficiaries : token:string -> A.Beneficiary.t list res
  ; add_beneficiary : token:string -> A.Beneficiary.Request.t -> A.Beneficiary.t res
  ; trust_beneficiary : token:string -> string -> A.Beneficiary.t res
  ; fee_quote : token:string -> A.Fee_quote.Request.t -> A.Fee_quote.t res
  ; create_transfer : token:string -> A.Transfer.Request.t -> A.Transfer.t res
  ; transfers : token:string -> A.Transfer.t list res
  ; transfer : token:string -> string -> A.Transfer.t res
  ; confirm_sca :
      token:string -> string -> A.Transfer.Confirm_request.t -> A.Transfer.t res
  ; restart_sca : token:string -> string -> A.Transfer.t res
  ; cancel_transfer : token:string -> string -> A.Transfer.t res
  ; swap_quote : token:string -> A.Swap.Quote_request.t -> A.Swap.t res
  ; execute_swap : token:string -> string -> A.Swap.t res
  ; swaps : token:string -> A.Swap.t list res
  ; cards : token:string -> A.Card.t list res
  ; issue_card : token:string -> A.Card.Issue_request.t -> A.Card.t res
  ; freeze_card : token:string -> string -> A.Card.t res
  ; unfreeze_card : token:string -> string -> A.Card.t res
  ; set_card_limits : token:string -> string -> A.Card.Limits.t -> A.Card.t res
  ; card_display : token:string -> string -> Hosted.Card_display.t res
  ; card_consent : token:string -> string -> challenge_id:string -> A.Sca_info.t res
  ; pan_token :
      token:string
      -> string
      -> [ `Token of A.Card.Pan_token.t | `Challenge of A.Sca_info.t ] res
  ; standing_orders : token:string -> A.Standing_order.t list res
  ; create_standing_order :
      token:string -> A.Standing_order.Request.t -> A.Standing_order.t res
  ; cancel_standing_order : token:string -> string -> A.Standing_order.t res
  ; provider_sca : token:string -> string -> A.Sca_confirm_request.t -> unit res
  ; reviews : ops_token:string -> A.Review.t list res
  ; decide_review :
      ops_token:string -> string -> A.Review.Decide_request.t -> A.Review.t res
  ; simulated_pan : string -> string option
  (** the digits the mock's stand-in card display shows; [None] when live *)
  ; subscribe : token:string -> on_push:(A.Ws_push.t -> unit) -> unit -> unit
  (** opens the push channel; the result closes it *)
  }

(** [base_url] without a trailing slash: ["http://localhost:8080"] or ["/api"]. Every
    [POST] outside [/auth] carries a fresh [Idempotency-Key]. *)
val live : base_url:string -> t

val mock : unit -> t

(** [?mock=1] selects {!mock}; [?api=<url>] the API at that URL; otherwise the API under
    [/api] on the same origin (how the compose and chart nginx serve it). *)
val of_query : (string * string) list -> t

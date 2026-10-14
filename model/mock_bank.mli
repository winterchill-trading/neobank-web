(** An in-memory bank behind [?mock=1]: every page works without a server. It follows the
    API's rules closely enough to exercise each screen (sessions with a 5-minute SCA
    window, KYC steps, balances and holds, fee quotes, review above a threshold, our SCA
    then a provider code for on-chain payments, swaps at fixed prices, cards, standing
    orders), but it is a demo, not a model of the API.

    Seeded with one verified customer, [ada@example.com] with the password
    [correct horse battery], and some history. Any 6-digit code is accepted except
    [000000], which is always wrong. *)

open! Core
open Neobank_api_types
module A := Api_types

type t
type 'a r = ('a, Api_failure.t) Result.t

val create : now:float -> t
val demo_email : string
val demo_password : string

(** {2 Sessions and KYC} *)

val signup : t -> now:float -> A.Auth.Signup_request.t -> A.Auth.Signup_response.t r
val login : t -> now:float -> A.Auth.Login_request.t -> A.Auth.Session.t r

val step_up
  :  t
  -> now:float
  -> token:string
  -> A.Auth.Step_up_request.t
  -> A.Auth.Session.t r

val logout : t -> token:string -> unit r
val me : t -> now:float -> token:string -> A.Me.t r
val kyc_status : t -> token:string -> A.Kyc.Status.t r

val kyc_start
  :  t
  -> now:float
  -> token:string
  -> A.Kyc.Start_request.t
  -> A.Kyc.Session.t r

(** Code steps take any good code; hosted steps take the code ["simulated"], which the
    mock's stand-in for the identity SDK sends when its flow ends. The last step approves
    the customer and opens the accounts. *)
val kyc_complete
  :  t
  -> now:float
  -> token:string
  -> A.Kyc.Complete_request.t
  -> A.Kyc.Status.t r

(** {2 Accounts} *)

val accounts : t -> token:string -> A.Account.t list r
val account : t -> token:string -> string -> A.Account.t r

(** [from] and [until] are [YYYY-MM-DD], inclusive; they default to the last 30 days. *)
val statement
  :  t
  -> now:float
  -> token:string
  -> ?from:string
  -> ?until:string
  -> string
  -> A.Statement.t r

(** An incoming payment, as the sandbox endpoint does it. *)
val deposit : t -> now:float -> token:string -> A.Sandbox.Deposit.t -> A.Transfer.t r

(** {2 Payments} *)

val beneficiaries : t -> token:string -> A.Beneficiary.t list r

val add_beneficiary
  :  t
  -> now:float
  -> token:string
  -> A.Beneficiary.Request.t
  -> A.Beneficiary.t r

val trust_beneficiary : t -> now:float -> token:string -> string -> A.Beneficiary.t r
val fee_quote : t -> token:string -> A.Fee_quote.Request.t -> A.Fee_quote.t r

val create_transfer
  :  t
  -> now:float
  -> token:string
  -> A.Transfer.Request.t
  -> A.Transfer.t r

val transfers : t -> token:string -> A.Transfer.t list r
val transfer : t -> token:string -> string -> A.Transfer.t r

val confirm_sca
  :  t
  -> now:float
  -> token:string
  -> string
  -> A.Transfer.Confirm_request.t
  -> A.Transfer.t r

val cancel_transfer : t -> now:float -> token:string -> string -> A.Transfer.t r

(** {2 Swaps} *)

val swap_quote : t -> now:float -> token:string -> A.Swap.Quote_request.t -> A.Swap.t r
val execute_swap : t -> now:float -> token:string -> string -> A.Swap.t r
val swaps : t -> token:string -> A.Swap.t list r

(** {2 Cards} *)

val cards : t -> token:string -> A.Card.t list r
val issue_card : t -> now:float -> token:string -> A.Card.Issue_request.t -> A.Card.t r
val freeze_card : t -> token:string -> string -> A.Card.t r
val unfreeze_card : t -> token:string -> string -> A.Card.t r

val set_card_limits
  :  t
  -> now:float
  -> token:string
  -> string
  -> A.Card.Limits.t
  -> A.Card.t r

val card_display : t -> token:string -> string -> Hosted.Card_display.t r

(** The challenge the (simulated) display component started; answer it with
    {!provider_sca}. *)
val card_consent
  :  t
  -> now:float
  -> token:string
  -> string
  -> challenge_id:string
  -> A.Sca_info.t r

val pan_token
  :  t
  -> now:float
  -> token:string
  -> string
  -> [ `Token of A.Card.Pan_token.t | `Challenge of A.Sca_info.t ] r

(** The digits the simulated display shows for a card; only the mock has them. *)
val simulated_pan : t -> string -> string option

(** {2 Standing orders and provider codes} *)

val standing_orders : t -> token:string -> A.Standing_order.t list r

val create_standing_order
  :  t
  -> now:float
  -> token:string
  -> A.Standing_order.Request.t
  -> A.Standing_order.t r

val cancel_standing_order : t -> token:string -> string -> A.Standing_order.t r

val provider_sca
  :  t
  -> now:float
  -> token:string
  -> string
  -> A.Sca_confirm_request.t
  -> unit r

(** {2 Operators} *)

val reviews : t -> A.Review.t list r
val decide_review : t -> now:float -> string -> A.Review.Decide_request.t -> A.Review.t r

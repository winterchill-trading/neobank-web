(** Checks run in the browser before a request goes out. They catch typing mistakes early;
    the API validates everything again and has the last word. Each returns the value in
    the form the API expects (an IBAN without spaces, an upper-case BIC, ...). *)

open! Core

(** ISO 13616: country code, two check digits, up to 30 letters or digits, and the
    mod-97 check. Spaces are dropped and letters upper-cased. *)
val iban : string -> string Or_error.t

(** 8 or 11 characters: bank (4 letters), country (2 letters), location, branch. *)
val bic : string -> string Or_error.t

(** A rough shape check per network: [BTC] (base58 [1]/[3] or bech32 [bc1]/[tb1]), [ETH]
    and [POLYGON] ([0x] and 40 hex digits), [SOL] (32 to 44 base58 characters), and
    [LIGHTNING] (a BOLT 11 invoice: [lnbc], [lntb] or [lnbcrt]). No checksums. *)
val address : network:string -> string -> string Or_error.t

val email : string -> string Or_error.t

(** Six digits from an authenticator app (spaces dropped). *)
val totp : string -> string Or_error.t

(** At least 12 characters (the API enforces its own rule too). *)
val password : string -> string Or_error.t

(** [YYYY-MM-DD]. *)
val date : string -> string Or_error.t

(** ISO 3166 alpha-2, upper-cased. *)
val country : string -> string Or_error.t

(** Not empty after trimming; [what] names the field in the error. *)
val required : what:string -> string -> string Or_error.t

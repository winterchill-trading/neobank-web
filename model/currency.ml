open! Core

type kind =
  | Fiat
  | Crypto
[@@deriving sexp, compare, equal]

type t =
  { code : string
  ; name : string
  ; exponent : int
  ; kind : kind
  ; symbol : string option
  ; networks : string list
  }
[@@deriving sexp, compare, equal]

let all =
  [ { code = "EUR"
    ; name = "Euro"
    ; exponent = 2
    ; kind = Fiat
    ; symbol = Some "€"
    ; networks = []
    }
  ; { code = "BTC"
    ; name = "Bitcoin"
    ; exponent = 8
    ; kind = Crypto
    ; symbol = None
    ; networks = [ "BTC"; "LIGHTNING" ]
    }
  ; { code = "ETH"
    ; name = "Ether"
    ; exponent = 18
    ; kind = Crypto
    ; symbol = None
    ; networks = [ "ETH" ]
    }
  ; { code = "USDC"
    ; name = "USD Coin"
    ; exponent = 6
    ; kind = Crypto
    ; symbol = None
    ; networks = [ "ETH"; "POLYGON"; "SOL" ]
    }
  ; { code = "USDT"
    ; name = "Tether"
    ; exponent = 6
    ; kind = Crypto
    ; symbol = None
    ; networks = [ "ETH"; "POLYGON" ]
    }
  ]
;;

let find code =
  List.find all ~f:(fun c -> String.Caseless.equal c.code code)
;;

let get code =
  match find code with
  | Some c -> c
  | None ->
    { code; name = code; exponent = 2; kind = Fiat; symbol = None; networks = [] }
;;

let is_crypto code =
  match (get code).kind with
  | Crypto -> true
  | Fiat -> false
;;

open! Core

let to_time ms = Time_ns.of_span_since_epoch (Time_ns.Span.of_ms ms)

let iso ms =
  let date, ofday = Time_ns.to_date_ofday (to_time ms) ~zone:Time_float.Zone.utc in
  let p = Time_ns.Ofday.to_parts ofday in
  sprintf "%sT%02d:%02d:%02d.%03dZ" (Date.to_string date) p.hr p.min p.sec p.ms
;;

let date ms = Date.to_string (Time_ns.to_date (to_time ms) ~zone:Time_float.Zone.utc)
let minute_ms = 60_000.
let day_ms = 86_400_000.

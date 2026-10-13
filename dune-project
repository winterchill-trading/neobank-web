(lang dune 3.17)

(name neobank-web)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/neobank-web))

(package
 (name neobank-web)
 (synopsis
  "Bonsai web app for the neobank demo: onboarding, accounts, payments, swaps and cards")
 (depends
  (ocaml
   (>= 5.2))
  ; bonsai_web runs ppx_css's css_inliner on %{deps}. Upstream dune 3.24.2, which the
  ; solver otherwise picks on a fresh switch, expands that to "./app.css", the inliner
  ; then writes "include ./app__generated" and bonsai_web fails with "Syntax error".
  ; The ox repository's dune 3.22.2+ox expands it to "app.css".
  (dune
   (< 3.23))
  core
  neobank-api-types
  bonsai_web
  virtual_dom
  async_kernel
  js_of_ocaml
  js_of_ocaml-ppx
  ppx_jane
  ppx_yojson_conv
  yojson
  zarith
  zarith_stubs_js))

# AGENTS.md (bin/)

`main.ml` is the entry point of the app, compiled by js_of_ocaml to `main.bc.js`. It reads the
mode from the query string (`?mock=1`, `?api=<url>`) and starts the components in `../ui`.

- Keep it to wiring. Page logic belongs in `../ui`.
- The installed static site is `index.html` plus `main.js`; check the bundle size after changes
  that add dependencies (release `main.js` is about 1.7 MB, 514 KB gzipped).

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/all`.

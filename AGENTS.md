# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `model/`, `ui/`, `bin/`, `smoke/`, `scripts/` and `test/`.
The workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`neobank-web` is the browser app of the neobank demo: sign-up and sign-in with an
authenticator, identity verification, accounts, payments, swaps, virtual cards, standing
orders, statements and an operator review queue. It is OCaml compiled to JavaScript with
js_of_ocaml and built with Bonsai. It talks only to `../neobank-api` and shares its request
and response types through the `neobank-api-types` package.

## Layout

| Path | Holds |
| --- | --- |
| `model/` | Plain OCaml, also built natively for the tests: routes, amounts in minor units, formatting, field checks (IBAN, BIC, addresses, Lightning invoices), statement CSV, QR encoder, refusals, and the mock bank |
| `ui/` | Bonsai components: one module per page, the backend (live over XHR or the mock), forms, the step-up flow |
| `bin/` | `main.ml`, the entry point compiled to `main.bc.js` |
| `static/` | `index.html` and the stylesheet (light and dark) |
| `smoke/` | The compiled app under node with a fake DOM: `run.js` (mock, every page), `live.js` (against a running API) |
| `scripts/` | `dev.sh` (build and serve), `live-smoke.sh` (starts the sibling API and runs `live.js`) |
| `test/` | Expect and Quickcheck tests of the model and the mock bank |
| `.github/` | CI workflow |

## Build, test, run

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch are required):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/all
opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/runtest
```

Parallel work: prefix the commands with `DUNE_BUILD_DIR=_build-neobank-web`.

Run locally: `scripts/dev.sh [-p PORT] [-r]` builds, assembles the site and serves it
(default port 8000; `-r` builds the release profile, which gives a smaller `main.js`).
Modes: `?mock=1` runs on the built-in mock bank; `?api=<url>` talks to a running API; with
no query the app expects the API under `/api` on its own origin.

Standalone: `opam install --deps-only --with-test .` needs `neobank-api-types` pinned from
`../neobank-api`, the same way CI does it.

## Conventions

- Every module has an `.mli` where it is part of a library. Core-style code: `open! Core`,
  labelled arguments, ppx_jane derivations.
- Amounts are minor units in Zarith, so 18-decimal ETH stays exact. Never convert through floats.
- Forms are `Bonsai.state_machine`s, not a single state map (see Pitfalls).
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root.

## Dependencies and dependents

- Depends on `neobank-api-types` (from `../neobank-api`), `bonsai_web`, `virtual_dom`,
  `js_of_ocaml`, `async_kernel`, `core`, `zarith`, `yojson`.
- CI pins only `neobank-api-types`; `bonsai_web` is an opam dependency that `ox-setup` installs.
- Used by the infra `web` image target, which installs `bin/main.bc.js` as the static site.

## CI

`ci.yml` sets up Node, runs the `ox-setup` action with the `neobank-api-types` pin, installs
`bonsai_web` if a restored switch lacks it, then `dune build`, `dune test` (which includes the
browser smoke on the mock), and the shared `obj-magic` workflow. Check a run with
`gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.

## Pitfalls

- A library module named `Validate` is shadowed by `open! Core`, which has its own `Validate`.
  Name it something else (`Check` is used).
- A form kept as one `Bonsai.state` map loses fields when two input events arrive before a
  render. Keep it in a `Bonsai.state_machine` whose action applies only the diff.
- The live smoke uses `-password-iterations 1000` and the memory store, so it finishes in about 5 s.
- The identity SDK and the card plugin are not exercised by tests; their script URLs are hard-coded
  from vendor documentation.
- The API coverage table in the README can lag `../neobank-api`; check the route before you
  document it as missing.

## Where to read more

- `README.md`: quick start, layout, pages, provider components, API coverage, tests.
- `../specs/repos/neobank-web.md`, and `../neobank-api/AGENTS.md` for the API side.
- `../oxcaml-claude-skills/skills/` (Bonsai and js_of_ocaml notes are in GOTCHAS.md, section
  "Bonsai web (js_of_ocaml)").

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>` first
  and stop if the plan touches any of them.
- Publish, tag or release, or point the smoke tests at a live provider.
- Put a credential in the bundle. The app reads none; provider ids come from the API at runtime.
- Print secrets, or poll CI in a loop.

# AGENTS.md (smoke/)

Browser smoke tests under node with a fake DOM.

- `run.js` runs the compiled app on the mock bank and walks 12 steps through every page. It runs in CI.
- `live.js` is skipped unless `API_URL` is set. `scripts/live-smoke.sh` builds the sibling
  `neobank-api`, starts it on the fake provider with the memory store, and runs `live.js`.
- Never point these at a live provider; they assume the fake one.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/runtest`.

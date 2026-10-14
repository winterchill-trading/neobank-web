#!/usr/bin/env bash
# Builds neobank-api from the sibling checkout, starts it on the fake provider with the
# memory store, and runs smoke/live.js (the compiled app over real HTTP) against it.
#
#   scripts/live-smoke.sh              from a workspace holding ../neobank-api
#   API_URL=http://host:port OPS_TOKEN=... scripts/live-smoke.sh    an API already running
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
ws=$(cd "$root/.." && pwd)
build_dir=${DUNE_BUILD_DIR:-$ws/_build}
port=${PORT:-18081}

(cd "$ws" && opam exec -- dune build --root . ./neobank-web/bin/main.bc.js)
app="$build_dir/default/neobank-web/bin/main.bc.js"

if [[ -z ${API_URL:-} ]]; then
  (cd "$ws" && opam exec -- dune build --root . ./neobank-api/bin/main.exe)
  state=$(mktemp -d)
  export OPS_TOKEN=${OPS_TOKEN:-live-smoke-ops}
  # Fewer PBKDF2 rounds keep sign-up and login fast; the API's default is much higher.
  NEOBANK_OPS_TOKEN=$OPS_TOKEN "$build_dir/default/neobank-api/bin/main.exe" \
    -port "$port" -store memory -state-dir "$state/state" -password-iterations 1000 \
    >"$state/api.log" 2>&1 &
  api=$!
  trap 'kill $api 2>/dev/null || true' EXIT
  for _ in $(seq 1 30); do
    curl -sf "http://localhost:$port/healthz" >/dev/null && break
    sleep 1
  done
  export API_URL=http://localhost:$port
fi

node "$root/smoke/live.js" "$app"

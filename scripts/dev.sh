#!/usr/bin/env bash
# Builds the app, assembles _site/ (index.html, main.js) and serves it.
#
#   scripts/dev.sh [-p PORT] [-r]     -r builds with the release profile (small main.js)
#
# Open http://localhost:PORT/?mock=1 for the built-in mock bank, or
# http://localhost:PORT/?api=http://localhost:8080 for a running neobank-api (its
# default CORS list allows http://localhost:8000).
set -euo pipefail

port=8000
profile=dev
while getopts "p:r" opt; do
  case $opt in
    p) port=$OPTARG ;;
    r) profile=release ;;
    *) exit 2 ;;
  esac
done

root=$(cd "$(dirname "$0")/.." && pwd)
build_dir=${DUNE_BUILD_DIR:-$root/_build}
cd "$root"
opam exec -- dune build --root . --profile "$profile" ./bin/main.bc.js

site=$root/_site
rm -rf "$site"
mkdir -p "$site"
cp static/index.html "$site/"
cp "$build_dir/default/bin/main.bc.js" "$site/main.js"
chmod -R u+w "$site"
echo "serving $site on http://localhost:$port/?mock=1"
exec python3 -m http.server "$port" --bind 127.0.0.1 --directory "$site"

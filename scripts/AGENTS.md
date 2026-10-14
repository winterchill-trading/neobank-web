# AGENTS.md (scripts/)

- `dev.sh [-p PORT] [-r]` builds the app, assembles `_site/` (`index.html`, `main.js`) and serves
  it. `-r` uses the release profile.
- `live-smoke.sh` builds `../neobank-api`, starts it on the fake provider and runs `smoke/live.js`
  against it.

Both are shell scripts: keep them passing `shellcheck`.

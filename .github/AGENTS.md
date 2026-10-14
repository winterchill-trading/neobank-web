# AGENTS.md (.github/)

`workflows/ci.yml` sets up Node, pins `neobank-api-types` from GitHub, installs `bonsai_web` on a
restored switch that lacks it, runs the build and tests, and calls the shared `obj-magic` workflow.

- The pin tracks the `neobank-api` package the types come from; a type change there needs this
  build to pass before its pin is moved.
- Check a run with `gh run list --limit 1`; at most one `gh run watch <id> --exit-status`.

# AGENTS.md (model/)

Plain OCaml with no browser dependency, so it also builds natively for the tests. It holds
routes, amounts, formatting, field checks, the statement CSV, the QR encoder, refusals and the
mock bank.

- Amounts are minor units in Zarith. Do not add a float path.
- `check.ml` holds the field validators (IBAN mod 97, BIC, addresses, Lightning invoices,
  codes); the tests flip single characters and expect a failure.
- `mock_bank.ml` must agree with the real API's refusals. When the API changes a refusal, change
  the mock in the same commit.
- Core shadows some module names (`Validate`); the model exposes its checks as `Check`.

Tests: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/runtest`.

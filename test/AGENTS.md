# AGENTS.md (test/)

Expect and Quickcheck tests of `../model`, run natively.

- Amounts round-trip for every currency; a single changed IBAN digit is always caught; the QR
  encoder matches the reference output on seven inputs.
- Scenario tests drive the mock bank: SCA attempts, provider codes, review, step-up, swaps, and
  sign-up to verified.
- Promote expect output with `dune promote <file>` for the one file that changed.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/runtest`.

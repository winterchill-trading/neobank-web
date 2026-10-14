# AGENTS.md (ui/)

Bonsai components. One module per page; `backend` chooses between the live XHR backend and the
mock; forms and the step-up flow are shared modules.

- Forms use `Bonsai.state_machine` with an action that applies only the diff (see the root
  Pitfalls). Do not keep a whole form in one `Bonsai.state`.
- Pages reach the API through the backend module, so the mock bank covers them.
- The provider components (identity SDK, card plugin) are loaded at runtime and are not
  covered by tests.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-web/all`.

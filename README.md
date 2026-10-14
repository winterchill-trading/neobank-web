# neobank-web

The browser app of the neobank demo: sign-up and sign-in with an authenticator, identity
verification, accounts with IBAN and deposit addresses, payments over SEPA, on-chain and
Lightning with strong customer authentication, swaps, virtual cards, standing orders,
statements, payees and an operator review queue. It is OCaml compiled to JavaScript with
js_of_ocaml and built with Bonsai, and it talks to
[neobank-api](https://github.com/winterchill-trading/neobank-api), whose request and
response types it shares (the `neobank-api-types` package).

## Quick start

```sh
scripts/dev.sh                # builds and serves on http://localhost:8000
```

- `http://localhost:8000/?mock=1` runs on the built-in mock bank: no server needed. Sign
  in as `demo@example.com` with the password `correct horse battery` and any 6-digit
  code (`000000` is always wrong).
- `http://localhost:8000/?api=http://localhost:8080` talks to a running neobank-api (its
  default CORS list allows `http://localhost:8000`).
- With no query the app expects the API under `/api` on its own origin, which is how
  the compose and chart nginx serve it (the infra `web` image target installs
  `share/neobank-web/{index.html,main.js}`).

`scripts/dev.sh -r` builds the release profile: `main.js` is 1.7 MB (514 KB gzipped);
the dev build is about 50 MB.

There are no credentials in this repository and the app reads none from files. The
card display component's application id and UI secret, and the identity SDK's access
token, come from the API at runtime.

## Layout

| Directory | What |
| --- | --- |
| `model/` | Plain OCaml, also built natively for the tests: routes, API paths, amounts in minor units (Zarith, so 18-decimal ETH stays exact), formatting, field checks (IBAN mod 97, BIC, addresses, Lightning invoices, codes), statement CSV, a QR encoder, refusals, and the mock bank |
| `ui/` | Bonsai components: one module per page, the backend (live over `XMLHttpRequest` or the mock), forms, the step-up flow, and the glue to the two provider components |
| `bin/` | The `main.bc.js` entry point and the installed static site |
| `static/` | `index.html` with the stylesheet (light and dark) |
| `smoke/` | The compiled app under node with a fake DOM: `run.js` walks every page on the mock, `live.js` against a running API |

## Pages

| Route | Page |
| --- | --- |
| `#/sign-up` | Details, then the authenticator key once, as a QR code |
| `#/sign-in` | Email, password and authenticator code (a login is SCA) |
| `#/onboarding` | KYC steps: email and mobile codes; document and liveness in the identity SDK; a sandbox approval button |
| `#/dashboard` | Balances per currency, recent activity |
| `#/accounts`, `#/accounts/:id` | Accounts; IBAN or deposit address with a QR code, the last 30 days, a sandbox deposit |
| `#/send`, `#/transfers/:id` | SEPA (standard or instant), crypto or Lightning with a fee quote; the payment page answers our challenge (password and code) and then the provider's code, and reloads until the payment is final |
| `#/swap` | Quote with a countdown, execute (fresh SCA), the provider's code when asked |
| `#/cards` | Issue a virtual card, freeze and unfreeze, limits, card details through the provider's component |
| `#/standing-orders` | Daily, weekly or monthly orders, confirmed with the provider's code |
| `#/statements` | Any account and period; CSV download built in the browser |
| `#/beneficiaries` | Payees with the IBAN check, the name check result and trust |
| `#/review` | Operators: two different operators approve a held payment, one may reject it (needs the ops token) |

Any action the API refuses with `sca_required` (its 5-minute window for sensitive
operations has passed) shows a password and code form in place; after the step-up the
customer presses the button again. Pages reload after every change and on every push
from the API's `/ws`.

## Provider components

Two pages embed the provider's own components; their scripts load only on those pages,
on first use (URLs in `model/hosted.ml`, glue in `ui/hosted_js.ml`):

- **Identity verification** runs in the KYC vendor's WebSDK 2.0, launched with the
  access token from `POST /kyc/start` for a hosted step and refreshed through the same
  route when it expires. Results reach the API by webhook, so the page polls the status.
- **Card number, CVV and PIN** are drawn by the provider's card display plugin in its own
  iframes. The plugin is created with the application id and UI secret from
  `GET /cards/:id/display` (the fake provider answers with no component, and the page
  stops after the token), asks for consent (`requestConsent`: the provider sends the
  customer a code), the API learns the challenge (`POST /cards/:id/consent`), the
  customer's code goes to `POST /sca/:id`, `POST /cards/:id/pan-token` returns the
  token, and the plugin renders with it. Neither this app nor the API ever sees the
  number.

A deployment with a Content-Security-Policy must allow those two script hosts in
`script-src` and `frame-src`. In mock mode both components are replaced by stand-ins.

## API coverage

| Route | Used by | In neobank-api |
| --- | --- | --- |
| `POST /auth/signup`, `/auth/login`, `/auth/step-up`, `/auth/logout` | sign-up, sign-in, step-up, sign-out | yes |
| `GET /me`, `GET /kyc`, `POST /kyc/start`, `POST /kyc/complete` | dashboard, onboarding | yes |
| `GET /accounts`, `GET /accounts/:id`, `GET /accounts/:id/statement` | accounts, statements | yes |
| `GET/POST /beneficiaries`, `POST /beneficiaries/:id/trust` | payees | yes |
| `POST /transfers/quote`, `GET/POST /transfers`, `GET /transfers/:id`, `POST /transfers/:id/confirm-sca`, `/sca`, `/cancel` | send, payment | yes |
| `POST /swaps/quote`, `GET /swaps`, `POST /swaps/:id/execute` | swap | yes |
| `GET/POST /cards`, `POST /cards/:id/freeze`, `/unfreeze`, `/limits`, `/pan-token` | cards | yes |
| `GET /cards/:id/display`, `POST /cards/:id/consent` | card details with the provider's plugin | yes |
| `GET/POST /standing-orders`, `DELETE /standing-orders/:id` | standing orders | yes |
| `POST /sca/:id` | provider codes for swaps, orders, card details | yes |
| `GET /ops/review`, `POST /ops/review/:id` | review queue | yes |
| `POST /sandbox/deposit`, `POST /sandbox/kyc-decision` | sandbox buttons | yes |
| `GET /ws?token=` | reload on pushes | yes |

Every `POST` outside `/auth` carries a fresh `Idempotency-Key`.

## Tests

`dune test` runs:

- expect and Quickcheck tests of the model: amounts round-trip through their text for
  every currency, one changed IBAN digit is always caught, the QR encoder matches the
  qrcodegen reference (version, mask and modules) on seven inputs up to version 22,
  and scenario tests of the mock bank (SCA attempts, provider codes, review, step-up,
  swaps, sign-up to verified);
- `smoke/run.js`: the compiled app on the mock, 12 steps through every page;
- `smoke/live.js`: skipped unless `API_URL` is set. `scripts/live-smoke.sh` builds the
  sibling neobank-api, starts it on the fake provider with the memory store and runs
  it: 9 steps over real HTTP (sign-up with computed authenticator codes, KYC with the
  sandbox decision, deposit, SEPA payment with SCA, swap, card with the provider's code,
  standing order, statement CSV, a held payment released by two operators).

What is verified where: everything above runs against the mock and against neobank-api
on its fake provider. The two provider components are not exercised by any test: the
smoke tests check that the identity SDK script is requested, but nothing here talks to
the real sandbox.

## Known gaps

- The API's hosted KYC session carries no SDK script URL, so the app uses the vendor's
  documented WebSDK URL.
- No test drives the real identity SDK or card plugin (they need a browser and the
  provider's sandbox).

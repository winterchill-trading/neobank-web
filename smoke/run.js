// Smoke test: runs the compiled app under node with fake_dom.js in mock mode (?mock=1)
// and walks every page: sign-in, accounts, payments with SCA, review, swaps, cards,
// standing orders, statements, payees, sign-up and onboarding.
//
//   node run.js <path to main.bc.js>
"use strict";
const path = require("path");
const { install, makeEvent } = require("./fake_dom.js");

const [, , appJs] = process.argv;
const env = install({ root: __dirname, hash: "", search: "?mock=1" });
const { document } = env;
const t0 = Date.now();
const steps = [];

const consoleErrors = [];
const origError = console.error;
console.error = (...args) => { consoleErrors.push(args.map(String).join(" ")); origError(...args); };
console.log = () => {}; // Bonsai prints a console banner at startup.
const print = (s) => process.stdout.write(s + "\n");

// Downloads go through URL.createObjectURL; keep the blobs to read them back.
const downloads = [];
URL.createObjectURL = (blob) => { downloads.push(blob); return "blob:fake/" + downloads.length; };
URL.revokeObjectURL = () => {};

const $ = (sel) => document.querySelector(sel);
const all = (sel) => document.querySelectorAll(sel);
const text = (el) => (el ? el.textContent : "");
const bodyText = () => text(document.body);
const byText = (sel, s) => all(sel).find((el) => text(el).trim().startsWith(s));
const input = (name) => all("input").find((i) => i.getAttribute("name") === name);
const select = (name) => all("select").find((i) => i.getAttribute("name") === name);

function fail(msg) {
  print("FAIL: " + msg);
  print("hash: " + location.hash);
  print("page: " + bodyText().slice(0, 2000));
  print("console errors: " + consoleErrors.join(" | "));
  if (process.env.SMOKE_DEBUG) print("html: " + document.body.outerHTML.slice(0, 6000));
  process.exit(1);
}

function waitFor(what, pred, timeoutMs = 4000) {
  return new Promise((resolve) => {
    const start = Date.now();
    const tick = () => {
      let v;
      try { v = pred(); } catch (e) { v = undefined; }
      if (v) return resolve(v);
      if (Date.now() - start > timeoutMs) fail("timed out waiting for " + what);
      setTimeout(tick, 10);
    };
    tick();
  });
}

function click(el, what) {
  if (!el) fail("nothing to click: " + what);
  if (el.disabled) fail("disabled: " + what);
  el.dispatchEvent(makeEvent("click"));
}

async function type(name, value) {
  const el = await waitFor("input " + name, () => input(name));
  el.value = value;
  el.dispatchEvent(makeEvent("input"));
  await waitFor("value of " + name, () => input(name) && input(name).value === value);
  // Let Bonsai render once, so the next click sees handlers built from this value.
  await new Promise((r) => setTimeout(r, 40));
}

async function choose(name, value) {
  const el = await waitFor("select " + name, () => select(name));
  el.value = value;
  el.dispatchEvent(makeEvent("change"));
  await new Promise((r) => setTimeout(r, 40));
}

async function go(hash, what, pred) {
  location.hash = hash;
  return waitFor(what, pred);
}

async function press(id) {
  const el = await waitFor("button #" + id, () => { const b = $("#" + id); return b && !b.disabled && b; });
  click(el, id);
}

function step(name) { steps.push(name); print(`ok ${steps.length} - ${name}`); }

async function signIn(email, password, code) {
  await go("#/sign-in", "sign-in form", () => input("email"));
  await type("email", email);
  await type("password", password);
  await type("totp", code);
  await press("sign-in");
}

async function main() {
  require(path.resolve(appJs));

  await waitFor("signed-out page", () => bodyText().includes("Sign in to see this page"));
  if (!text($("#backend")).includes("mock data")) fail("header should say mock data");
  step("signed out: the dashboard asks to sign in; the header names the mock");

  await signIn("demo@example.com", "correct horse battery", "000000");
  await waitFor("wrong code", () => bodyText().includes("the email, password or code is wrong"));
  await type("totp", "123456");
  await press("sign-in");
  await waitFor("dashboard", () => location.hash === "#/dashboard" && $("#balance-EUR"));
  if (!text($("#balance-EUR")).includes("€2,189.00")) fail("EUR balance " + text($("#balance-EUR")));
  if (!text($("#balance-EUR")).includes("on hold €18.90")) fail("held amount missing");
  if (!$("#activity") || $("#activity").querySelectorAll("tr").length < 4) fail("activity table");
  step("sign-in with a wrong then a good code; balances and recent activity");

  await go("#/accounts", "accounts table", () => $("#accounts"));
  await go("#/accounts/acc_eur_cus_demo", "account page", () => $("#account") && $("path"));
  if (!bodyText().includes("DE62 3704 0044 0532 0130 01")) fail("IBAN not shown in groups");
  const d = $("path").getAttribute("d");
  if (!d.startsWith("M0 0h7v1h-7z")) fail("QR path should start with the finder pattern: " + d.slice(0, 30));
  await type("deposit", "100");
  await press("simulate-deposit");
  await waitFor("balance after deposit", () => bodyText().includes("€2,289.00"));
  step("account page: grouped IBAN, QR code, sandbox deposit updates the balance");

  await go("#/send", "send form", () => $("#send") && select("account"));
  await choose("account", "acc_eur_cus_demo");
  await choose("payee", "ben_landlord");
  await choose("rail", "sepa_instant");
  await type("amount", "100");
  await type("reference", "Deposit");
  await press("get-quote");
  await waitFor("quote", () => $("#quote") && text($("#quote")).includes("€0.25"));
  await press("send-submit");
  await waitFor("transfer page", () => location.hash.startsWith("#/transfers/") && $("#confirm-sca"));
  await type("password", "correct horse battery");
  await type("code", "000000");
  await press("confirm-submit");
  await waitFor("wrong code", () => bodyText().includes("the code is wrong") && bodyText().includes("4 attempts left"));
  await type("code", "123456");
  await press("confirm-submit");
  await waitFor("settled", () => text($("#transfer")).includes("Settled"));
  step("SEPA Instant: fee quote, wrong code counted, confirmed with password and code, settled");

  await go("#/send", "send form", () => $("#send") && select("account"));
  await choose("account", "acc_eur_cus_demo");
  await choose("payee", "ben_landlord");
  await type("amount", "1500");
  await press("get-quote");
  await waitFor("quote", () => $("#quote"));
  await press("send-submit");
  await waitFor("under review", () => location.hash.startsWith("#/transfers/") && text($("#transfer")).includes("Under review"));
  const held = location.hash;
  await go("#/review", "review login", () => input("ops_token"));
  await type("ops_token", "ops");
  await press("review-open");
  const item = await waitFor("review item", () => all("div").find((e) => e.id && e.id.startsWith("review-rev")));
  await press("approve-" + item.id.slice("review-".length));
  await waitFor("queue empty", () => bodyText().includes("Nothing to review."));
  await go(held, "released payment", () => $("#confirm-sca"));
  step("a large payment waits for review; the operator approves it; it asks for SCA");

  await go("#/send", "send form", () => $("#kind-crypto"));
  click($("#kind-crypto"), "crypto tab");
  await choose("account", "acc_btc_cus_demo");
  await choose("payee", "ben_cold");
  await type("amount", "0.001");
  await press("get-quote");
  await waitFor("BTC quote", () => $("#quote") && text($("#quote")).includes("0.00002 BTC"));
  await press("send-submit");
  await waitFor("crypto confirm", () => $("#confirm-sca"));
  await type("password", "correct horse battery");
  await type("code", "111111");
  await press("confirm-submit");
  await waitFor("provider code", () => $("#confirm-provider"));
  await type("code", "222222");
  await press("provider-submit");
  await waitFor("settled", () => text($("#transfer")).includes("Settled"));
  step("on-chain BTC: our SCA, then the provider's code, settled");

  await go("#/swap", "swap form", () => $("#swap") && select("from"));
  await choose("from", "acc_btc_cus_demo");
  await choose("to", "acc_eur_cus_demo");
  await type("sell", "0.001");
  await press("swap-quote-button");
  await waitFor("swap quote", () => $("#swap-quote") && text($("#swap-quote")).includes("€59.70"));
  await press("swap-execute");
  await waitFor("swapped", () => bodyText().includes("Swapped.") && $("#swaps"));
  step("swap BTC to EUR at the quoted price");

  await go("#/cards", "cards", () => $("#card-card_1"));
  await press("reveal-card_1");
  await waitFor("PAN code", () => $("#pan-code"));
  await type("pan-code", "333333");
  await press("pan-code-submit");
  await waitFor("simulated PAN", () => $("#pan-simulated") && text($("#pan-simulated")).includes("4242 4242 4242 4242"));
  await press("freeze-card_1");
  await waitFor("frozen", () => $("#unfreeze-card_1"));
  await press("unfreeze-card_1");
  await waitFor("active again", () => $("#freeze-card_1"));
  await press("edit-limits-card_1");
  await type("monthly", "4000");
  await press("limits-save");
  await waitFor("limits saved", () => bodyText().includes("Limits saved.") && text($("#card-card_1")).includes("€4,000.00"));
  await choose("card_account", "acc_eur_cus_demo");
  await press("issue-card-submit");
  await waitFor("second card", () => all("div").filter((e) => e.classList.contains("bank-card")).length === 2);
  step("cards: consent code, simulated details, freeze and unfreeze, limits, a new card");

  await go("#/standing-orders", "standing orders", () => $("#standing-orders") && select("payee"));
  await choose("account", "acc_eur_cus_demo");
  await choose("payee", "ben_landlord");
  await type("amount", "25");
  await type("day", "15");
  await type("start", new Date(Date.now() + 86400000 * 3).toISOString().slice(0, 10));
  await press("standing-order-submit");
  const code = await waitFor("order code", () => all("input").find((i) => (i.getAttribute("name") || "").startsWith("code-sca")));
  await type(code.getAttribute("name"), "444444");
  await press("provider-code-" + code.getAttribute("name").slice("code-".length));
  await waitFor("order active", () => !$("#provider-codes") && text($("#standing-orders")).includes("Monthly on day 15"));
  step("standing order with the provider's code");

  await go("#/statements", "statements", () => $("#statements") && select("account"));
  await choose("account", "acc_eur_cus_demo");
  await press("statement-load");
  await waitFor("statement", () => $("#statement") && text($("#statement")).includes("Salary September"));
  await press("statement-csv");
  await waitFor("csv download", () => downloads.length === 1);
  const csv = await downloads[0].text();
  if (!csv.startsWith("at,amount,currency,code,reference,balance\r\n")) fail("csv header: " + csv.slice(0, 60));
  if (!csv.includes(",3200.00,EUR,sepa_in,Salary September,3200.00")) fail("csv line missing");
  step("statement and its CSV download");

  await go("#/beneficiaries", "payees", () => $("#payees"));
  await type("name", "Second Account");
  await type("destination", "DE89 3704 0044 0532 0130 99");
  await waitFor("IBAN error", () => bodyText().includes("check digits do not match"));
  await type("destination", "DE89 3704 0044 0532 0130 00");
  await press("add-payee-submit");
  await waitFor("payee added", () => bodyText().includes("The name matches") && text($("#payees")).includes("Second Account"));
  step("payees: the IBAN check catches a typo; a new payee with a name check");

  await press("sign-out");
  await waitFor("signed out", () => location.hash === "#/sign-in");
  await go("#/sign-up", "sign-up form", () => input("given_name"));
  for (const [k, v] of [["email", "sam@example.com"], ["password", "a long enough password"], ["given_name", "Sam"], ["family_name", "Example"], ["date_of_birth", "1990-04-01"], ["nationality", "de"], ["mobile", "+4915100000000"], ["line1", "Street 1"], ["city", "Berlin"], ["postal_code", "10115"], ["country", "DE"]]) await type(k, v);
  await press("sign-up");
  await waitFor("enrolment", () => $("#enrol") && $("path"));
  if (!bodyText().includes("JBSWY3DPEHPK3PXP")) fail("authenticator key missing");
  await press("go-sign-in");
  await signIn("sam@example.com", "a long enough password", "123456");
  await waitFor("onboarding", () => location.hash === "#/onboarding" && $("#kyc-start-email"));
  for (const s of ["email", "mobile"]) {
    await press("kyc-start-" + s);
    await waitFor("code entry " + s, () => $("#kyc-code"));
    await type("code", "555555");
    await press("kyc-submit-code");
    await waitFor(s + " done", () => !$("#kyc-start-" + s) && !$("#kyc-code"));
  }
  for (const s of ["identity_document", "liveness"]) {
    await press("kyc-start-" + s);
    await waitFor("simulated SDK " + s, () => $("#kyc-simulated"));
    await press("kyc-simulate");
    await waitFor(s + " done", () => !$("#kyc-start-" + s) && !$("#kyc-simulated"));
  }
  await waitFor("verified", () => text($("#kyc-status")).includes("You are verified"));
  await go("#/dashboard", "new balances", () => $("#balance-EUR") && text($("#balance-EUR")).includes("€0.00"));
  step("sign-up, authenticator enrolment, onboarding steps, accounts opened");

  if (consoleErrors.length) fail("console errors: " + consoleErrors.join(" | "));
  print(`all ${steps.length} steps passed in ${Date.now() - t0} ms`);
  process.exit(0);
}

process.on("uncaughtException", (e) => fail("uncaught " + (e && e.stack ? e.stack : e)));
process.on("unhandledRejection", (e) => fail("unhandled " + (e && e.stack ? e.stack : e)));
setTimeout(() => fail("smoke test took longer than 90 s"), 90_000).unref();
main();

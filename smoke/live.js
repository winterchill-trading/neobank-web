// Live smoke test: the compiled app (?api=http://api.test) against a running neobank-api
// on the fake provider. Every request goes over real HTTP to API_URL; the walk signs up,
// enrols the authenticator (codes computed here, RFC 6238), verifies, deposits, pays
// with SCA, swaps, issues a card, sets up a standing order, downloads a statement and
// has an operator release a held payment.
//
//   API_URL=http://localhost:8080 OPS_TOKEN=... node live.js <path to main.bc.js>
"use strict";
const path = require("path");
const crypto = require("crypto");
const { install, makeEvent } = require("./fake_dom.js");

const [, , appJs] = process.argv;
const API_URL = process.env.API_URL;
const OPS_TOKEN = process.env.OPS_TOKEN || "";
const PROVIDER_CODE = process.env.PROVIDER_CODE || "123456";
if (!API_URL) { process.stdout.write("live smoke skipped: set API_URL to a running neobank-api\n"); process.exit(0); }

const requests = [];
const handler = async (method, url, headers, body) => {
  const real = url.replace("http://api.test", API_URL);
  const res = await fetch(real, { method, headers, body: body == null ? undefined : body });
  const text = await res.text();
  requests.push(`${method} ${url.replace("http://api.test", "")} ${res.status}`);
  return { status: res.status, body: text };
};
const env = install({ root: __dirname, hash: "", search: "?api=http://api.test", handler });
const { document } = env;
// The push socket goes to the same API.
const RealWebSocket = globalThis.WebSocket;
globalThis.WebSocket = class extends RealWebSocket {
  constructor(url) { super(url.replace("ws://api.test", API_URL.replace(/^http/, "ws"))); }
};
const downloads = [];
URL.createObjectURL = (blob) => { downloads.push(blob); return "blob:fake/" + downloads.length; };
URL.revokeObjectURL = () => {};

const consoleErrors = [];
const origError = console.error;
console.error = (...args) => { consoleErrors.push(args.map(String).join(" ")); origError(...args); };
console.log = () => {};
const print = (s) => process.stdout.write(s + "\n");
const t0 = Date.now();
const steps = [];

const $ = (sel) => document.querySelector(sel);
const all = (sel) => document.querySelectorAll(sel);
const text = (el) => (el ? el.textContent : "");
const bodyText = () => text(document.body);
const input = (name) => all("input").find((i) => i.getAttribute("name") === name);
const select = (name) => all("select").find((i) => i.getAttribute("name") === name);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function fail(msg) {
  print("FAIL: " + msg);
  print("hash: " + location.hash);
  print("page: " + bodyText().slice(0, 2000));
  print("requests: " + requests.slice(-15).join(", "));
  print("console errors: " + consoleErrors.join(" | "));
  process.exit(1);
}

function waitFor(what, pred, timeoutMs = 15000) {
  return new Promise((resolve) => {
    const start = Date.now();
    const tick = () => {
      let v;
      try { v = pred(); } catch (e) { v = undefined; }
      if (v) return resolve(v);
      if (Date.now() - start > timeoutMs) fail("timed out waiting for " + what);
      setTimeout(tick, 25);
    };
    tick();
  });
}

async function type(name, value) {
  const el = await waitFor("input " + name, () => input(name));
  el.value = value;
  el.dispatchEvent(makeEvent("input"));
  await sleep(40);
}

async function choose(name, value) {
  const el = await waitFor("select " + name, () => select(name));
  el.value = value;
  el.dispatchEvent(makeEvent("change"));
  await sleep(40);
}

async function press(id) {
  const el = await waitFor("button #" + id, () => { const b = $("#" + id); return b && !b.disabled && b; });
  el.dispatchEvent(makeEvent("click"));
}

// Leaves and comes back, so the page loads its data again.
async function reload(hash, what, pred) {
  for (let i = 0; i < 40; i++) {
    location.hash = "#/sign-in-not-a-page";
    await sleep(30);
    location.hash = hash;
    const start = Date.now();
    while (Date.now() - start < 500) {
      try { if (pred()) return; } catch (e) {}
      await sleep(25);
    }
  }
  fail("timed out reloading for " + what);
}

function totp(secret, offset) {
  const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
  let bits = "";
  for (const c of secret.replace(/=+$/, "")) bits += alphabet.indexOf(c).toString(2).padStart(5, "0");
  const key = Buffer.from(bits.match(/.{8}/g).map((b) => parseInt(b, 2)));
  const counter = Buffer.alloc(8);
  counter.writeBigUInt64BE(BigInt(Math.floor(Date.now() / 30000) + offset));
  const mac = crypto.createHmac("sha1", key).update(counter).digest();
  const o = mac[19] & 15;
  return String((mac.readUInt32BE(o) & 0x7fffffff) % 1000000).padStart(6, "0");
}

function step(name) { steps.push(name); print(`ok ${steps.length} - ${name}`); }

async function main() {
  require(path.resolve(appJs));
  const email = `web-${Date.now()}@example.com`;
  const password = "correct horse battery staple";

  location.hash = "#/sign-up";
  await waitFor("sign-up form", () => input("given_name"));
  for (const [k, v] of [["email", email], ["password", password], ["given_name", "Web"], ["family_name", "Customer"], ["date_of_birth", "1990-01-01"], ["nationality", "DE"], ["mobile", "+4930000000"], ["line1", "1 Test Street"], ["city", "Berlin"], ["postal_code", "10115"], ["country", "DE"]]) await type(k, v);
  await press("sign-up");
  await waitFor("enrolment", () => $("#enrol"));
  const secret = text(all("span").find((s) => s.classList.contains("selectable"))).trim();
  if (!/^[A-Z2-7]+=*$/.test(secret)) fail("authenticator key " + secret);
  await press("go-sign-in");
  await waitFor("sign-in form", () => input("totp"));
  await type("email", email);
  await type("password", password);
  await type("totp", totp(secret, -1));
  await press("sign-in");
  await waitFor("onboarding", () => location.hash === "#/onboarding" && $("#kyc-start-email"));
  step("sign-up, authenticator key, sign-in with a computed code");

  for (const s of ["email", "mobile"]) {
    await press("kyc-start-" + s);
    await waitFor("code entry " + s, () => $("#kyc-code"));
    await type("code", PROVIDER_CODE);
    await press("kyc-submit-code");
    await waitFor(s + " done", () => !$("#kyc-code") && !$("#kyc-start-" + s));
  }
  await press("kyc-start-identity_document");
  await waitFor("hosted step", () => $("#kyc-sdk") && $("script"));
  const sdk = all("script").map((s) => s.src || s.getAttribute("src")).join(" ");
  if (!sdk.includes("sns-websdk-builder.js")) fail("identity SDK script not requested: " + sdk);
  // The fake provider decides once both hosted steps have started.
  await waitFor("identity step open", () => bodyText().includes("Photograph your ID document in the window below"));
  await press("kyc-start-liveness");
  await waitFor("liveness step open", () => bodyText().includes("Take a short selfie video in the window below"));
  await press("kyc-sandbox-approve");
  await waitFor("verified", () => text($("#kyc-status")).includes("You are verified"));
  step("KYC: email and mobile codes, the identity SDK script requested, sandbox approval");

  await reload("#/accounts", "accounts with an IBAN", () => $("#accounts") && /[A-Z]{2}\d{2} \d{4}/.test(text($("#accounts"))));
  const eur = all("a").find((a) => (a.getAttribute("href") || "").startsWith("#/accounts/") && text(a).startsWith("Euro"));
  const eurHash = eur.getAttribute("href");
  location.hash = eurHash;
  await waitFor("account page", () => $("#account") && $("path"));
  await type("deposit", "1000");
  await press("simulate-deposit");
  await reload(eurHash, "balance", () => $("#account") && text($("#account")).includes("€1,000.00"));
  step("accounts opened with an IBAN and QR; a sandbox deposit of €1,000.00 arrives");

  location.hash = "#/beneficiaries";
  await waitFor("payees", () => $("#add-payee"));
  await type("name", "Payee");
  await type("destination", "FR1420041010050500013M02606");
  await press("add-payee-submit");
  await waitFor("payee", () => $("#payees") && text($("#payees")).includes("Payee"));
  location.hash = "#/send";
  await waitFor("send", () => $("#send") && select("account"));
  const eurId = eurHash.split("/").pop();
  await choose("account", eurId);
  const payeeId = select("payee").childNodes.map((o) => o.value).find((v) => v);
  await choose("payee", payeeId);
  await type("amount", "30");
  await type("reference", "Invoice 42");
  await press("get-quote");
  await waitFor("quote", () => $("#quote"));
  await press("send-submit");
  await waitFor("confirm", () => $("#confirm-sca"));
  await type("password", password);
  await type("code", totp(secret, 0));
  await press("confirm-submit");
  await waitFor("provider code or settled", () => $("#confirm-provider") || text($("#transfer")).includes("Settled"), 20000);
  if ($("#confirm-provider")) {
    await type("code", PROVIDER_CODE);
    await press("provider-submit");
  }
  await waitFor("settled", () => text($("#transfer")).includes("Settled"), 30000);
  step("SEPA payment: payee, fee quote, password and authenticator code, settled");

  location.hash = "#/swap";
  await waitFor("swap", () => $("#swap") && select("from"));
  const btcId = select("to").childNodes.map((o) => o.value).find((v) => v && v.toLowerCase().includes("btc")) ||
    select("to").childNodes.filter((o) => text(o).startsWith("Bitcoin")).map((o) => o.value)[0];
  await choose("from", eurId);
  await choose("to", btcId);
  await type("sell", "100");
  await press("swap-quote-button");
  await waitFor("swap quote", () => $("#swap-quote"));
  await press("swap-execute");
  await waitFor("swap result", () => bodyText().includes("Swapped.") || bodyText().includes("Swap sent") || $("#provider-codes"), 20000);
  if ($("#provider-codes") || bodyText().includes("code to confirm")) {
    await reload("#/swap", "swap code", () => $("#provider-codes"));
    const code = all("input").find((i) => (i.getAttribute("name") || "").startsWith("code-"));
    await type(code.getAttribute("name"), PROVIDER_CODE);
    await press("provider-code-" + code.getAttribute("name").slice("code-".length));
    await waitFor("code accepted", () => bodyText().includes("Code accepted."));
  }
  await reload("#/swap", "executed swap", () => $("#swaps") && text($("#swaps")).includes("Executed"));
  step("swap EUR to BTC at the provider's quote");

  location.hash = "#/cards";
  await waitFor("cards", () => $("#issue-card"));
  await choose("card_account", eurId);
  await press("issue-card-submit");
  const card = await waitFor("card", () => all("div").find((d) => d.classList.contains("bank-card")));
  const cardId = card.id.slice("card-".length);
  await press("freeze-" + cardId);
  await waitFor("frozen", () => $("#unfreeze-" + cardId));
  await press("unfreeze-" + cardId);
  await waitFor("active", () => $("#freeze-" + cardId));
  await press("edit-limits-" + cardId);
  await type("daily", "200");
  await press("limits-save");
  await waitFor("limits saved", () => bodyText().includes("Limits saved."));
  await press("reveal-" + cardId);
  await waitFor("card details step", () => $("#pan-code") || bodyText().includes("display token") || bodyText().includes("shows no card details"), 20000);
  if ($("#pan-code")) {
    await type("pan-code", PROVIDER_CODE);
    await press("pan-code-submit");
  }
  await waitFor("token", () => bodyText().includes("issued a display token") || bodyText().includes("shows no card details"), 20000);
  step("card: issue, freeze, unfreeze, limits, display token after the provider's code");

  location.hash = "#/standing-orders";
  await waitFor("orders", () => $("#new-standing-order") && select("payee"));
  await choose("account", eurId);
  await choose("payee", payeeId);
  await type("amount", "10");
  await type("day", "1");
  await type("start", new Date().toISOString().slice(0, 10));
  await press("standing-order-submit");
  await waitFor("order created", () => bodyText().includes("Standing order set up.") || bodyText().includes("enter the code"));
  if (bodyText().includes("enter the code")) {
    const code = await waitFor("order code", () => all("input").find((i) => (i.getAttribute("name") || "").startsWith("code-")));
    await type(code.getAttribute("name"), PROVIDER_CODE);
    await press("provider-code-" + code.getAttribute("name").slice("code-".length));
    await waitFor("accepted", () => bodyText().includes("Code accepted."));
  }
  await waitFor("order listed", () => $("#standing-orders") && text($("#standing-orders")).includes("Monthly on day 1"));
  step("standing order");

  location.hash = "#/statements";
  await waitFor("statements", () => $("#statements") && select("account"));
  await choose("account", eurId);
  await press("statement-load");
  await waitFor("statement", () => $("#statement"));
  await press("statement-csv");
  await waitFor("download", () => downloads.length === 1);
  const csv = await downloads[0].text();
  if (!csv.startsWith("at,amount,currency,code,reference,balance")) fail("csv " + csv.slice(0, 80));
  step("statement and CSV");

  if (OPS_TOKEN) {
    location.hash = "#/send";
    await waitFor("send", () => $("#send") && select("account"));
    await choose("account", eurId);
    await choose("payee", payeeId);
    await type("amount", "600");
    await press("get-quote");
    await waitFor("quote", () => $("#quote"));
    await press("send-submit");
    await waitFor("held", () => location.hash.startsWith("#/transfers/") && text($("#transfer")).includes("Under review"));
    const held = location.hash;
    location.hash = "#/review";
    await waitFor("review login", () => input("ops_token"));
    await type("ops_token", OPS_TOKEN);
    await press("review-open");
    const item = await waitFor("review item", () => all("div").find((e) => e.id && e.id.startsWith("review-") && e.classList.contains("review-item")));
    await press("approve-" + item.id.slice("review-".length));
    await waitFor("queue updated", () => !$("#" + item.id));
    location.hash = held;
    await waitFor("confirm after release", () => $("#confirm-sca"), 20000);
    await type("password", password);
    await type("code", totp(secret, 1));
    await press("confirm-submit");
    await waitFor("provider code or settled", () => $("#confirm-provider") || text($("#transfer")).includes("Settled"), 20000);
    if ($("#confirm-provider")) {
      await type("code", PROVIDER_CODE);
      await press("provider-submit");
    }
    await waitFor("settled", () => text($("#transfer")).includes("Settled"), 30000);
    step("a held payment: operator approval, then the customer's SCA");
  }

  if (consoleErrors.length) fail("console errors: " + consoleErrors.join(" | "));
  print(`all ${steps.length} live steps passed in ${Date.now() - t0} ms; ${requests.length} API requests`);
  process.exit(0);
}

process.on("uncaughtException", (e) => fail("uncaught " + (e && e.stack ? e.stack : e)));
process.on("unhandledRejection", (e) => fail("unhandled " + (e && e.stack ? e.stack : e)));
setTimeout(() => fail("live smoke took longer than 180 s"), 180_000).unref();
main();

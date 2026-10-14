// A minimal DOM for running the compiled app under node: enough of Node, Element,
// document, window, location (with hashchange) and XMLHttpRequest (served from local
// files) for Bonsai to mount, render and handle clicks. Not a browser; see run.js.
"use strict";
const fs = require("fs");
const path = require("path");

const missing = new Set();
const debug = process.env.FAKE_DOM_DEBUG === "1";

class EventTarget_ {
  constructor() { this._listeners = {}; }
  addEventListener(type, fn) { (this._listeners[type] ||= []).push(fn); }
  removeEventListener(type, fn) {
    const l = this._listeners[type];
    if (l) this._listeners[type] = l.filter((f) => f !== fn);
  }
  dispatchEvent(ev) {
    ev.target ||= this;
    let node = this;
    while (node && !ev._stopped) {
      ev.currentTarget = node;
      for (const fn of (node._listeners && node._listeners[ev.type]) || []) fn.call(node, ev);
      const prop = node["on" + ev.type];
      if (typeof prop === "function") prop.call(node, ev);
      node = ev.bubbles ? node.parentNode : null;
    }
    return !ev.defaultPrevented;
  }
}

function makeEvent(type, init = {}) {
  return {
    type, bubbles: init.bubbles !== false, cancelable: true, defaultPrevented: false,
    _stopped: false, button: 0, buttons: 0, clientX: 0, clientY: 0, timeStamp: Date.now(),
    key: init.key, shiftKey: false, ctrlKey: false, altKey: false, metaKey: false,
    preventDefault() { this.defaultPrevented = true; },
    stopPropagation() { this._stopped = true; },
    stopImmediatePropagation() { this._stopped = true; },
    ...init,
  };
}

class Node_ extends EventTarget_ {
  constructor(nodeType, nodeName, doc) {
    super();
    this.nodeType = nodeType;
    this.nodeName = nodeName;
    this.ownerDocument = doc;
    this.childNodes = [];
    this.parentNode = null;
  }
  get firstChild() { return this.childNodes[0] || null; }
  get lastChild() { return this.childNodes[this.childNodes.length - 1] || null; }
  get parentElement() { return this.parentNode && this.parentNode.nodeType === 1 ? this.parentNode : null; }
  _sibling(d) {
    if (!this.parentNode) return null;
    const s = this.parentNode.childNodes;
    return s[s.indexOf(this) + d] || null;
  }
  get nextSibling() { return this._sibling(1); }
  get previousSibling() { return this._sibling(-1); }
  get isConnected() {
    let n = this;
    while (n.parentNode) n = n.parentNode;
    return n === this.ownerDocument;
  }
  _detach(child) {
    if (child.parentNode) {
      const s = child.parentNode.childNodes;
      s.splice(s.indexOf(child), 1);
      child.parentNode = null;
    }
  }
  appendChild(child) { return this.insertBefore(child, null); }
  insertBefore(child, ref) {
    if (child.nodeType === 11) {
      for (const c of [...child.childNodes]) this.insertBefore(c, ref);
      return child;
    }
    this._detach(child);
    const i = ref ? this.childNodes.indexOf(ref) : -1;
    if (i < 0) this.childNodes.push(child); else this.childNodes.splice(i, 0, child);
    child.parentNode = this;
    return child;
  }
  removeChild(child) { this._detach(child); return child; }
  replaceChild(next, prev) { this.insertBefore(next, prev); this._detach(prev); return prev; }
  remove() { if (this.parentNode) this.parentNode.removeChild(this); }
  replaceWith(n) { if (this.parentNode) this.parentNode.replaceChild(n, this); }
  contains(n) { while (n) { if (n === this) return true; n = n.parentNode; } return false; }
  hasChildNodes() { return this.childNodes.length > 0; }
  get textContent() { return this.childNodes.map((c) => c.textContent).join(""); }
  set textContent(v) {
    for (const c of [...this.childNodes]) this._detach(c);
    if (v !== "" && v != null) this.appendChild(this.ownerDocument.createTextNode(String(v)));
  }
  getRootNode() { let n = this; while (n.parentNode) n = n.parentNode; return n; }
}

class CharacterData_ extends Node_ {
  constructor(type, name, data, doc) { super(type, name, doc); this.data = String(data); }
  get nodeValue() { return this.data; }
  set nodeValue(v) { this.data = String(v); }
  get textContent() { return this.nodeType === 3 ? this.data : ""; }
  set textContent(v) { this.data = String(v); }
  get length() { return this.data.length; }
  replaceData(offset, count, s) { this.data = this.data.slice(0, offset) + s + this.data.slice(offset + count); }
}

function makeStyle() {
  const style = {
    setProperty(k, v) { this[k] = v; },
    removeProperty(k) { delete this[k]; },
    getPropertyValue(k) { return this[k] || ""; },
  };
  return style;
}

class Element_ extends Node_ {
  constructor(tag, doc, ns) {
    super(1, tag.toUpperCase(), doc);
    this.tagName = tag.toUpperCase();
    this.localName = tag.toLowerCase();
    this.namespaceURI = ns || "http://www.w3.org/1999/xhtml";
    this._attrs = new Map();
    this.style = makeStyle();
    this.value = "";
    this.checked = false;
    this.disabled = false;
    this.scrollTop = 0;
    this.scrollLeft = 0;
    const self = this;
    this.classList = {
      add(...c) { const s = new Set(self.className.split(/\s+/).filter(Boolean)); c.forEach((x) => s.add(x)); self.className = [...s].join(" "); },
      remove(...c) { self.className = self.className.split(/\s+/).filter((x) => x && !c.includes(x)).join(" "); },
      contains(c) { return self.className.split(/\s+/).includes(c); },
      toggle(c) { if (this.contains(c)) { this.remove(c); return false; } this.add(c); return true; },
    };
  }
  get className() { return this._attrs.get("class") || ""; }
  set className(v) { this._attrs.set("class", String(v)); }
  get id() { return this._attrs.get("id") || ""; }
  set id(v) { this._attrs.set("id", String(v)); }
  get children() { return this.childNodes.filter((c) => c.nodeType === 1); }
  get attributes() { return [...this._attrs].map(([name, value]) => ({ name, value })); }
  // Reflect the attributes the app sets that browsers mirror as properties.
  setAttribute(k, v) {
    this._attrs.set(k, String(v));
    if (k === "value") this.value = String(v);
    if (k === "disabled") this.disabled = true;
  }
  setAttributeNS(_ns, k, v) { this.setAttribute(k, v); }
  getAttribute(k) { return this._attrs.has(k) ? this._attrs.get(k) : null; }
  removeAttribute(k) { this._attrs.delete(k); if (k === "disabled") this.disabled = false; }
  removeAttributeNS(_ns, k) { this._attrs.delete(k); }
  hasAttribute(k) { return this._attrs.has(k); }
  focus() { this.ownerDocument.activeElement = this; }
  blur() { this.ownerDocument.activeElement = this.ownerDocument.body; }
  click() { this.dispatchEvent(makeEvent("click")); }
  getBoundingClientRect() { return { x: 0, y: 0, top: 0, left: 0, right: 0, bottom: 0, width: 0, height: 0 }; }
  scrollIntoView() {}
  matches(sel) { return matches(this, sel); }
  closest(sel) { let n = this; while (n && n.nodeType === 1) { if (matches(n, sel)) return n; n = n.parentNode; } return null; }
  querySelectorAll(sel) {
    const out = [];
    const walk = (n) => { for (const c of n.childNodes) if (c.nodeType === 1) { if (matches(c, sel)) out.push(c); walk(c); } };
    walk(this);
    return out;
  }
  querySelector(sel) { return this.querySelectorAll(sel)[0] || null; }
  getElementsByTagName(t) { return this.querySelectorAll(t); }
  get innerHTML() { return this.childNodes.map(serialize).join(""); }
  set innerHTML(v) { this.textContent = ""; if (v) this.appendChild(this.ownerDocument.createTextNode(String(v))); }
  get outerHTML() { return serialize(this); }
}

// Selectors: "tag", "#id", ".class", "tag.class", "[attr]" and "[attr=value]".
function matches(el, sel) {
  const m = /^([a-zA-Z0-9-]*)(?:#([\w-]+))?(?:\.([\w-]+))?(?:\[([\w-]+)(?:="?([^"\]]*)"?)?\])?$/.exec(sel.trim());
  if (!m) throw new Error("fake_dom: unsupported selector " + sel);
  const [, tag, id, cls, attr, val] = m;
  if (tag && el.localName !== tag.toLowerCase()) return false;
  if (id && el.id !== id) return false;
  if (cls && !el.classList.contains(cls)) return false;
  if (attr && (!el.hasAttribute(attr) || (val !== undefined && el.getAttribute(attr) !== val))) return false;
  return true;
}

function serialize(n) {
  if (n.nodeType === 3) return n.data;
  if (n.nodeType !== 1) return "";
  const attrs = n.attributes.map((a) => ` ${a.name}="${a.value}"`).join("");
  return `<${n.localName}${attrs}>${n.childNodes.map(serialize).join("")}</${n.localName}>`;
}

class Document_ extends Node_ {
  constructor() {
    super(9, "#document", null);
    this.ownerDocument = this;
    this.readyState = "complete";
    this.documentElement = this.createElement("html");
    this.appendChild(this.documentElement);
    this.head = this.createElement("head");
    this.body = this.createElement("body");
    this.documentElement.appendChild(this.head);
    this.documentElement.appendChild(this.body);
    this.activeElement = this.body;
    this.visibilityState = "visible";
    this.hidden = false;
    this.cookie = "";
    this.compatMode = "CSS1Compat";
  }
  createElement(tag) { return new Element_(tag, this); }
  createElementNS(ns, tag) { return new Element_(tag, this, ns); }
  createTextNode(t) { return new CharacterData_(3, "#text", t, this); }
  createComment(t) { return new CharacterData_(8, "#comment", t, this); }
  createDocumentFragment() { return new Node_(11, "#document-fragment", this); }
  createEvent() { const ev = makeEvent(""); ev.initEvent = (type, bubbles) => { ev.type = type; ev.bubbles = bubbles; }; return ev; }
  getElementById(id) { return this.documentElement.querySelector("#" + id); }
  querySelector(s) { return this.documentElement.querySelector(s); }
  querySelectorAll(s) { return this.documentElement.querySelectorAll(s); }
  getElementsByTagName(t) { return this.documentElement.querySelectorAll(t); }
  hasFocus() { return true; }
}

function wrapDebug(obj, label) {
  if (!debug) return obj;
  return new Proxy(obj, {
    get(t, k, r) {
      if (typeof k === "string" && !(k in t) && !missing.has(label + "." + k)) {
        missing.add(label + "." + k);
        console.error("fake_dom: missing " + label + "." + k);
      }
      return Reflect.get(t, k, r);
    },
  });
}

// Serves XMLHttpRequests: [handler(method, url, headers, body)] may answer with
// {status, body}, or a promise of one (a real HTTP call); otherwise the url is read as a
// file relative to [root].
function makeXhr(root, handler) {
  return class FakeXMLHttpRequest {
    constructor() { this.readyState = 0; this.status = 0; this._headers = {}; this.responseType = ""; this.upload = {}; }
    open(method, url) { this._method = method; this._url = url; this.readyState = 1; }
    setRequestHeader(k, v) { this._headers[k] = v; }
    overrideMimeType() {}
    getResponseHeader() { return "application/json"; }
    getAllResponseHeaders() { return "content-type: application/json\r\n"; }
    abort() {}
    addEventListener(type, fn) { this["on" + type] = fn; }
    send(body) {
      setTimeout(async () => {
        const answer = handler && (await handler(this._method, this._url, this._headers, body));
        if (answer) {
          this.status = answer.status;
          this.responseText = typeof answer.body === "string" ? answer.body : JSON.stringify(answer.body);
        } else {
          const file = path.join(root, decodeURIComponent(this._url.split("?")[0]));
          try {
            this.responseText = fs.readFileSync(file, "utf8");
            this.status = 200;
          } catch (e) {
            this.responseText = JSON.stringify({ error: "not found: " + this._url });
            this.status = 404;
          }
        }
        this.response = this.responseText;
        this.statusText = this.status === 200 ? "OK" : "Not Found";
        this.readyState = 4;
        FakeXMLHttpRequest.log.push(`${this._method} ${this._url} ${this.status}`);
        if (this.onreadystatechange) this.onreadystatechange();
        if (this.onload) this.onload();
        if (this.onloadend) this.onloadend();
      }, 1);
    }
  };
}

function install({ root, hash = "", search = "", handler }) {
  const document = new Document_();
  document.defaultView = globalThis;
  const app = document.createElement("div");
  app.id = "app";
  document.body.appendChild(app);
  const win = globalThis;
  const listeners = new EventTarget_();
  let currentHash = hash;
  const location = {
    get hash() { return currentHash; },
    set hash(v) {
      const next = v.startsWith("#") ? v : "#" + v;
      if (next === currentHash) return;
      currentHash = next;
      setTimeout(() => win.dispatchEvent(makeEvent("hashchange", { bubbles: false })), 0);
    },
    get href() { return "http://localhost/" + search + currentHash; },
    search,
    pathname: "/",
    port: "",
    assign() {},
    replace() {},
    origin: "http://localhost",
    protocol: "http:",
    host: "localhost",
    hostname: "localhost",
    reload() {},
  };
  const storage = () => {
    const m = new Map();
    return { getItem: (k) => (m.has(k) ? m.get(k) : null), setItem: (k, v) => m.set(k, String(v)), removeItem: (k) => m.delete(k), clear: () => m.clear() };
  };
  const globals = {
    window: win,
    self: win,
    document: wrapDebug(document, "document"),
    location,
    history: { pushState() {}, replaceState() {}, state: null, length: 1 },
    navigator: { userAgent: "node-fake-dom", language: "en", platform: "node" },
    localStorage: storage(),
    sessionStorage: storage(),
    addEventListener: listeners.addEventListener.bind(listeners),
    removeEventListener: listeners.removeEventListener.bind(listeners),
    dispatchEvent(ev) {
      ev.target ||= win;
      for (const fn of listeners._listeners[ev.type] || []) fn.call(win, ev);
      const prop = win["on" + ev.type];
      if (typeof prop === "function") prop.call(win, ev);
      return true;
    },
    requestAnimationFrame: (f) => setTimeout(() => f(performance.now()), 16),
    cancelAnimationFrame: (id) => clearTimeout(id),
    requestIdleCallback: (f) => setTimeout(() => f({ timeRemaining: () => 10, didTimeout: false }), 1),
    cancelIdleCallback: (id) => clearTimeout(id),
    getComputedStyle: () => makeStyle(),
    matchMedia: () => ({ matches: false, addListener() {}, removeListener() {}, addEventListener() {}, removeEventListener() {} }),
    innerWidth: 1280,
    innerHeight: 800,
    devicePixelRatio: 1,
    XMLHttpRequest: makeXhr(root, handler),
    Node: Node_,
    Element: Element_,
    HTMLElement: Element_,
    Text: CharacterData_,
    Document: Document_,
    MutationObserver: class { observe() {} disconnect() {} takeRecords() { return []; } },
    ResizeObserver: class { observe() {} unobserve() {} disconnect() {} },
    IntersectionObserver: class { observe() {} unobserve() {} disconnect() {} },
    alert: (m) => console.log("alert: " + m),
  };
  // defineProperty, because node 21+ defines some of these (navigator) as getters.
  for (const [k, v] of Object.entries(Object.getOwnPropertyDescriptors(globals)))
    Object.defineProperty(win, k, { ...v, configurable: true });
  win.XMLHttpRequest.log = [];
  return { document, app, makeEvent, missing };
}

module.exports = { install, makeEvent, serialize };

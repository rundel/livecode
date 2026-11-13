/*
 * livecode's page script. It opens a websocket to the server that served
 * the page and applies each JSON frame it receives:
 *
 *   lang       Prism language of the file (sent on connect)
 *   content    the whole file as text ("" clears the view)
 *   selection  instructor's selected lines, such as "3-5,9" ("" clears)
 *   messages   array of Noty options to show as toasts
 *   interval   seconds until the next update, drawn as a progress circle
 *   status     state of the session, one of the keys of STATUS_TEXT: the
 *              server sends "ended" when it stops, and a tailcat follower's
 *              local server sends the state of its link to the broadcast
 *   route      how a tailcat follower reaches the broadcast, as
 *              {kind: "direct" | "relay" | "", relay: "<relay region>"}
 *              (sent by a tailcat follower's local server; "" is unknown)
 *
 * The socket reconnects with backoff, and the status bar says so while the
 * page is disconnected. After "ended" the bar says that the session is over
 * instead; the socket is still retried, quietly, so the page picks up again
 * if the instructor restarts the server.
 *
 * Page options come from data attributes on <body>:
 *   data-message-mode  "html" (default) shows message text as HTML, "text"
 *                      shows it as plain text
 */
(function () {
  "use strict";

  var RECONNECT_MIN_MS = 1000;
  var RECONNECT_MAX_MS = 30000;

  var STATUS_TEXT = {
    connecting: "",
    connected: "",
    reconnecting: "Disconnected, reconnecting…",
    waiting: "Connecting to the broadcast…",
    ended: "The session has ended."
  };

  var state = {
    view: null,
    socket: null,
    retryDelay: RECONNECT_MIN_MS,
    retryTimer: null,
    ended: false,
    options: { messageMode: "html" }
  };

  function report(what, err) {
    if (window.console && console.error) console.error("livecode: " + what + " failed", err);
  }

  function escapeHtml(text) {
    return String(text)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function prefersReducedMotion() {
    return !!(window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches);
  }

  function setStatus(status) {
    var el = document.getElementById("livecode-status");
    if (!el) return;
    state.ended = status === "ended";
    if (state.ended) clearProgress();
    el.setAttribute("data-status", status);
    el.textContent = STATUS_TEXT[status] || "";
  }

  // A dropped connection after the session ended is expected, not news.
  function setDisconnected() {
    if (!state.ended) setStatus("reconnecting");
  }

  var ROUTE_TEXT = {
    direct: ["direct", "Connected directly to the instructor's computer"],
    relay: ["relay", "Connected through a tailcat relay"]
  };

  function setRoute(route) {
    var el = document.getElementById("livecode-route");
    var label = document.getElementById("livecode-route-label");
    if (!el || !label) return;
    var kind = route && typeof route === "object" ? route.kind : "";
    if (!Object.prototype.hasOwnProperty.call(ROUTE_TEXT, kind)) {
      el.hidden = true;
      el.removeAttribute("data-route");
      return;
    }
    var relay = kind === "relay" && typeof route.relay === "string" && /^[A-Za-z0-9-]{1,16}$/.test(route.relay)
      ? route.relay : "";
    var text = ROUTE_TEXT[kind][0] + (relay ? " " + relay : "");
    var title = ROUTE_TEXT[kind][1] + (relay ? " (" + relay + ")" : "");
    el.setAttribute("data-route", kind);
    el.setAttribute("title", title);
    el.setAttribute("aria-label", title);
    label.textContent = text;
    el.hidden = false;
  }

  function clearProgress() {
    var pb = document.getElementById("progressbar");
    if (!pb) return;
    while (pb.lastChild) pb.removeChild(pb.lastChild);
  }

  function drawProgress(interval) {
    clearProgress();
    var pb = document.getElementById("progressbar");
    if (!pb || !window.ProgressBar || prefersReducedMotion()) return;
    var seconds = Number(interval);
    if (!(seconds > 0)) return;
    new window.ProgressBar.Circle(pb, {
      strokeWidth: 50,
      easing: "linear",
      duration: seconds * 1000,
      color: "#6499D3",
      svgStyle: null
    }).animate(1.0);
  }

  function showMessage(msg) {
    if (!msg || typeof msg !== "object" || !window.Noty) return;
    var options = {};
    Object.keys(msg).forEach(function (key) { options[key] = msg[key]; });
    if (state.options.messageMode === "text") options.text = escapeHtml(msg.text);
    if (prefersReducedMotion() && !options.animation) options.animation = { open: null, close: null };
    new window.Noty(options).show();
  }

  // Applied in this order, each step on its own, so a bad message or option
  // can never keep the code from updating.
  var STEPS = [
    ["status", function (status) {
      if (typeof status !== "string" || !Object.prototype.hasOwnProperty.call(STATUS_TEXT, status)) {
        throw new TypeError("unknown status");
      }
      setStatus(status);
    }],
    ["route", setRoute],
    ["lang", function (lang) { state.view.setLanguage(lang); }],
    ["content", function (content) {
      if (typeof content !== "string") throw new TypeError("content is not a string");
      state.view.setContent(content);
    }],
    ["selection", function (selection) {
      if (typeof selection !== "string") throw new TypeError("selection is not a string");
      state.view.setSelection(selection);
    }],
    ["messages", function (messages) {
      if (!Array.isArray(messages)) throw new TypeError("messages is not an array");
      messages.forEach(function (msg) {
        try {
          showMessage(msg);
        } catch (err) {
          report("message", err);
        }
      });
    }],
    ["interval", function (interval) {
      if (!state.ended) drawProgress(interval);
    }]
  ];

  function handleFrame(data) {
    var frame;
    try {
      frame = JSON.parse(data);
    } catch (err) {
      report("parsing a frame", err);
      return;
    }
    if (!frame || typeof frame !== "object") return;

    STEPS.forEach(function (step) {
      if (!Object.prototype.hasOwnProperty.call(frame, step[0])) return;
      try {
        step[1](frame[step[0]]);
      } catch (err) {
        report(step[0], err);
      }
    });
  }

  // The socket keeps the page's directory: a proxy in front of the server,
  // such as Posit Workbench's, routes by that path.
  function socketUrl() {
    var loc = window.location;
    var dir = loc.pathname.replace(/[^/]*$/, "");
    return (loc.protocol === "https:" ? "wss://" : "ws://") + loc.host + dir;
  }

  function scheduleReconnect() {
    if (state.retryTimer !== null) return;
    var delay = state.retryDelay;
    state.retryDelay = Math.min(state.retryDelay * 2, RECONNECT_MAX_MS);
    state.retryTimer = window.setTimeout(function () {
      state.retryTimer = null;
      connect();
    }, delay);
  }

  function connect() {
    var ws;
    try {
      ws = new WebSocket(socketUrl());
    } catch (err) {
      report("opening the connection", err);
      setDisconnected();
      scheduleReconnect();
      return;
    }
    state.socket = ws;

    ws.onopen = function () {
      state.retryDelay = RECONNECT_MIN_MS;
      setStatus("connected");
    };
    ws.onmessage = function (event) {
      handleFrame(event.data);
    };
    ws.onclose = function () {
      if (state.socket !== ws) return;
      state.socket = null;
      clearProgress();
      setRoute(null);
      setDisconnected();
      scheduleReconnect();
    };
  }

  // Coming back online (or back to the tab) should not wait out the backoff.
  function retryNow() {
    if (state.socket !== null || state.retryTimer === null) return;
    window.clearTimeout(state.retryTimer);
    state.retryTimer = null;
    connect();
  }

  function start() {
    var mode = document.body.getAttribute("data-message-mode");
    if (mode === "text" || mode === "html") state.options.messageMode = mode;

    state.view = window.livecodeCodeView.create(document.getElementById("livecode-view"));
    window.livecode = { view: state.view, handleFrame: handleFrame, setStatus: setStatus, settings: null };

    // The settings are optional: if they fail, the page still updates.
    try {
      window.livecode.settings = window.livecodeSettings.mount(state.view, document);
    } catch (err) {
      report("display settings", err);
    }

    window.addEventListener("online", retryNow);
    document.addEventListener("visibilitychange", function () {
      if (document.visibilityState === "visible") retryNow();
    });

    setStatus("connecting");
    connect();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", start);
  } else {
    start();
  }
})();

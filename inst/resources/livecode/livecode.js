/*
 * livecode's page script. It opens a websocket to the server that served
 * the page and applies each JSON frame it receives:
 *
 *   lang       Prism language of the file (sent on connect)
 *   content    the whole file as text ("" clears the view)
 *   selection  instructor's selected lines, such as "3-5,9" ("" clears)
 *   interval   seconds until the next update, drawn as a progress ring
 *   status     state of the session, one of the keys of STATUS_TEXT: the
 *              server sends "ended" when it stops. A tailcat follower's
 *              local server sends the state of its link to the broadcast,
 *              "finished" when the broadcast was stopped (the follower is
 *              still running and says how to stop it), and "stopped" when
 *              the follower itself is stopped
 *   route      how a tailcat follower reaches the broadcast, as
 *              {kind: "direct" | "relay" | "", relay: "<relay region>"}
 *              (sent by a tailcat follower's local server; "" is unknown)
 *
 * The socket reconnects with backoff, and the status bar says so while the
 * page is disconnected. After "ended", "finished", or "stopped" the bar says
 * that the session is over instead; the socket is still retried, quietly, so the page picks up again
 * if the instructor restarts the server.
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
    ended: "The session has ended.",
    finished: "The session has ended. Run livecode::unfollow() in R to close this page.",
    stopped: "Stopped following."
  };

  var OVER = { ended: true, finished: true, stopped: true };

  var state = {
    view: null,
    socket: null,
    retryDelay: RECONNECT_MIN_MS,
    retryTimer: null,
    ended: false,
    progress: []
  };

  function report(what, err) {
    if (window.console && console.error) console.error("livecode: " + what + " failed", err);
  }

  function prefersReducedMotion() {
    return !!(window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches);
  }

  function setStatus(status) {
    var el = document.getElementById("livecode-status");
    if (!el) return;
    state.ended = OVER[status] === true;
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

  // The ring fills over the time until the next update, easing off to a
  // dimmer full ring at the end so the restart is not a hard jump, and the
  // center dot beats as each update lands. A late update leaves the ring
  // full and dim, which reads as waiting.
  var RING_LENGTH = 2 * Math.PI * 6;

  function clearProgress() {
    state.progress.forEach(function (animation) { animation.cancel(); });
    state.progress = [];
    var pb = document.getElementById("progressbar");
    if (pb) pb.removeAttribute("data-active");
  }

  function drawProgress(interval) {
    clearProgress();
    var pb = document.getElementById("progressbar");
    var seconds = Number(interval);
    if (!pb || !(seconds > 0) || prefersReducedMotion()) return;
    var arc = pb.querySelector(".livecode-progress-arc");
    var dot = pb.querySelector(".livecode-progress-dot");
    if (!arc || !dot || typeof arc.animate !== "function") return;

    pb.setAttribute("data-active", "");
    state.progress = [
      arc.animate([
        { strokeDashoffset: RING_LENGTH, opacity: 1 },
        { opacity: 1, offset: 0.8 },
        { strokeDashoffset: 0, opacity: 0.45 }
      ], { duration: seconds * 1000, easing: "linear", fill: "forwards" }),
      dot.animate([
        { transform: "scale(1)" },
        { transform: "scale(1.6)", offset: 0.35 },
        { transform: "scale(1)" }
      ], { duration: Math.min(450, seconds * 1000), easing: "ease-out" })
    ];
  }

  // Applied in this order, each step on its own, so a bad field can never
  // keep the code from updating.
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

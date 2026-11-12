/*
 * Per-viewer display settings: code font size, following the instructor's
 * cursor, and showing the highlighted lines. They last for the page's
 * lifetime only; every load starts from the defaults.
 *
 * Loading this file only defines window.livecodeSettings; mount() wires the
 * controls in page.html to a code view, so the pure helpers can be tested
 * without a DOM.
 */
(function (global) {
  "use strict";

  // Matches --lc-code-size in livecode-code-view.css.
  var DEFAULT_SIZE = 13;
  var MIN_SIZE = 9;
  var MAX_SIZE = 32;

  function clampSize(size) {
    var n = typeof size === "string" && size.trim() !== "" ? Number(size) : size;
    if (typeof n !== "number" || !isFinite(n)) return DEFAULT_SIZE;
    return Math.min(MAX_SIZE, Math.max(MIN_SIZE, Math.round(n)));
  }

  function stepSize(size, delta) {
    return clampSize(clampSize(size) + delta);
  }

  function mount(view, doc) {
    var root = doc.getElementById("livecode-settings");
    var toggle = doc.getElementById("livecode-settings-toggle");
    var panel = doc.getElementById("livecode-settings-panel");
    var sizeValue = doc.getElementById("livecode-size-value");
    var follow = doc.getElementById("livecode-follow");
    var highlight = doc.getElementById("livecode-highlight");
    var sizeButtons = root.querySelectorAll("[data-size]");
    var settings = { fontSize: DEFAULT_SIZE, follow: true, highlight: true };

    function apply(changed) {
      if (!changed || changed === "fontSize") view.setFontSize(settings.fontSize);
      if (!changed || changed === "highlight") view.setHighlight(settings.highlight);
      if (!changed || changed === "follow") view.setFollow(settings.follow);

      sizeValue.textContent = settings.fontSize + "px";
      for (var i = 0; i < sizeButtons.length; i++) {
        var kind = sizeButtons[i].getAttribute("data-size");
        var atLimit = (kind === "-1" && settings.fontSize <= MIN_SIZE) ||
          (kind === "1" && settings.fontSize >= MAX_SIZE) ||
          (kind === "reset" && settings.fontSize === DEFAULT_SIZE);
        // aria-disabled rather than disabled, so a focused button keeps focus.
        sizeButtons[i].setAttribute("aria-disabled", atLimit ? "true" : "false");
      }
      follow.checked = settings.follow;
      highlight.checked = settings.highlight;
      // With no highlight there is nothing to follow.
      follow.disabled = !settings.highlight;
    }

    function update(key, value) {
      if (settings[key] === value) return;
      settings[key] = key === "fontSize" ? clampSize(value) : !!value;
      apply(key);
    }

    function setOpen(open, refocus) {
      panel.hidden = !open;
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
      if (!open && refocus) toggle.focus();
    }

    function isOpen() {
      return !panel.hidden;
    }

    toggle.addEventListener("click", function () {
      setOpen(!isOpen(), false);
    });

    Array.prototype.forEach.call(sizeButtons, function (button) {
      button.addEventListener("click", function () {
        var kind = button.getAttribute("data-size");
        update("fontSize", kind === "reset" ? DEFAULT_SIZE : stepSize(settings.fontSize, Number(kind)));
      });
    });

    follow.addEventListener("change", function () { update("follow", follow.checked); });
    highlight.addEventListener("change", function () { update("highlight", highlight.checked); });

    doc.addEventListener("keydown", function (event) {
      if (!isOpen() || (event.key !== "Escape" && event.key !== "Esc")) return;
      setOpen(false, root.contains(doc.activeElement));
    });
    doc.addEventListener("pointerdown", function (event) {
      if (isOpen() && !root.contains(event.target)) setOpen(false, false);
    });
    root.addEventListener("focusout", function (event) {
      if (isOpen() && event.relatedTarget && !root.contains(event.relatedTarget)) setOpen(false, false);
    });

    apply(null);
    root.hidden = false;

    return {
      get: function () {
        return { fontSize: settings.fontSize, follow: settings.follow, highlight: settings.highlight };
      },
      set: function (key, value) { update(key, value); },
      open: function (open) { setOpen(!!open, false); }
    };
  }

  global.livecodeSettings = {
    DEFAULT_SIZE: DEFAULT_SIZE,
    MIN_SIZE: MIN_SIZE,
    MAX_SIZE: MAX_SIZE,
    clampSize: clampSize,
    stepSize: stepSize,
    mount: mount
  };
})(typeof window !== "undefined" ? window : this);

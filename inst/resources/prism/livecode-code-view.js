/*
 * livecode's code view, on Prism (adapted from markermd's code view).
 *
 * The text is highlighted once with Prism, then split into one .lc-line
 * element per source line. Line numbers are drawn on those elements and the
 * instructor's selection is a class on them (.lc-hl), so a selection change
 * only toggles classes and never re-highlights. No Prism plugins are used,
 * so leading blank lines and indentation are kept and the numbers are the
 * real source line numbers.
 *
 * Loading this file only defines window.livecodeCodeView; it does not touch
 * the page, so the pure helpers can be tested without a DOM.
 */
(function (global) {
  "use strict";

  // Prism highlights synchronously, so very long texts are shown plain.
  var MAX_HIGHLIGHT_CHARS = 400000;
  // After the viewer scrolls by hand, the view stops following the
  // instructor's selection for this long.
  var USER_SCROLL_GRACE_MS = 5000;

  // A single trailing newline ends the last line rather than starting an
  // empty one, matching how editors and readLines() count lines.
  function normalizeText(text) {
    text = String(text).replace(/\r\n?/g, "\n");
    if (text.slice(-1) === "\n") text = text.slice(0, -1);
    return text;
  }

  // Split Prism's HTML into lines, closing the open token spans at each line
  // break and reopening them on the next line, so every line is valid HTML
  // on its own.
  function splitLines(html) {
    var lines = [];
    var current = "";
    var open = [];
    var re = /(<span[^>]*>)|(<\/span>)|([^<]+)/g;
    var m;
    while ((m = re.exec(html)) !== null) {
      if (m[1]) {
        open.push(m[1]);
        current += m[1];
      } else if (m[2]) {
        open.pop();
        current += m[2];
      } else {
        var parts = m[3].split("\n");
        for (var i = 0; i < parts.length; i++) {
          if (i > 0) {
            for (var j = 0; j < open.length; j++) current += "</span>";
            lines.push(current);
            current = open.join("");
          }
          current += parts[i];
        }
      }
    }
    lines.push(current);
    return lines;
  }

  // "3-5,9" -> [[3, 5], [9, 9]]. Anything malformed is skipped.
  function parseSelection(value) {
    if (typeof value !== "string" || value === "") return [];
    var ranges = [];
    value.split(",").forEach(function (part) {
      var m = /^\s*(\d+)\s*(?:-\s*(\d+))?\s*$/.exec(part);
      if (!m) return;
      var start = Number(m[1]);
      var end = m[2] === undefined ? start : Number(m[2]);
      if (end < start) {
        var tmp = start;
        start = end;
        end = tmp;
      }
      if (start >= 1) ranges.push([start, end]);
    });
    return ranges;
  }

  function prefersReducedMotion() {
    return !!(global.matchMedia && global.matchMedia("(prefers-reduced-motion: reduce)").matches);
  }

  // root: the scrolling element holding <pre><code>; its data-language is
  // the initial language.
  function CodeView(root) {
    var self = this;
    this.root = root;
    this.code = root.querySelector("code");
    this.language = root.getAttribute("data-language") || "none";
    this.text = null;
    this.selection = "";
    this.lastUserScroll = -Infinity;

    var markUserScroll = function () { self.lastUserScroll = Date.now(); };
    ["wheel", "touchmove", "mousedown", "keydown"].forEach(function (type) {
      root.addEventListener(type, markUserScroll, { passive: true });
    });
  }

  CodeView.prototype.setLanguage = function (language) {
    language = typeof language === "string" && language ? language : "none";
    if (language === this.language) return;
    this.language = language;
    this.root.setAttribute("data-language", language);
    if (this.text !== null) this.render();
  };

  CodeView.prototype.setContent = function (text) {
    this.text = String(text);
    this.render();
  };

  CodeView.prototype.setSelection = function (selection) {
    selection = typeof selection === "string" ? selection : "";
    if (selection === this.selection) return;
    this.selection = selection;
    this.mark();
    this.follow();
  };

  CodeView.prototype.render = function () {
    var text = normalizeText(this.text === null ? "" : this.text);
    var code = this.code;
    var Prism = global.Prism;
    var grammar = Prism && Prism.languages && Prism.languages[this.language];
    var n;

    if (text === "") {
      code.textContent = "";
      n = 0;
    } else if (grammar && text.length <= MAX_HIGHLIGHT_CHARS) {
      // Prism encodes the text itself, so this HTML holds no markup from
      // the source.
      var lines = splitLines(Prism.highlight(text, grammar, this.language));
      code.innerHTML = lines.map(function (line, i) {
        return '<span class="lc-line" data-n="' + (i + 1) + '">' + line + "</span>";
      }).join("");
      n = lines.length;
    } else {
      var doc = code.ownerDocument;
      var fragment = doc.createDocumentFragment();
      var plain = text.split("\n");
      plain.forEach(function (line, i) {
        var el = doc.createElement("span");
        el.className = "lc-line";
        el.setAttribute("data-n", String(i + 1));
        el.textContent = line;
        fragment.appendChild(el);
      });
      code.textContent = "";
      code.appendChild(fragment);
      n = plain.length;
    }

    this.root.style.setProperty("--lc-gutter", (String(Math.max(n, 1)).length + 1) + "ch");
    this.mark();
  };

  // Marks the selected lines and returns the first one (or null).
  CodeView.prototype.mark = function () {
    var lines = this.code.querySelectorAll(".lc-line.lc-hl");
    for (var i = 0; i < lines.length; i++) lines[i].classList.remove("lc-hl");

    var all = this.code.children;
    var first = null;
    parseSelection(this.selection).forEach(function (range) {
      var end = Math.min(range[1], all.length);
      for (var n = range[0]; n <= end; n++) {
        all[n - 1].classList.add("lc-hl");
        if (!first || n < Number(first.getAttribute("data-n"))) first = all[n - 1];
      }
    });
    return first;
  };

  // Scrolls the first selected line into view, unless it is already visible
  // or the viewer scrolled by hand recently.
  CodeView.prototype.follow = function () {
    if (Date.now() - this.lastUserScroll < USER_SCROLL_GRACE_MS) return;
    var first = this.code.querySelector(".lc-line.lc-hl");
    if (!first) return;

    var view = this.root.getBoundingClientRect();
    var line = first.getBoundingClientRect();
    if (line.top >= view.top && line.bottom <= view.bottom) return;

    var top = Math.max(0, this.root.scrollTop + line.top - view.top - 2 * line.height);
    var behavior = prefersReducedMotion() ? "auto" : "smooth";
    if (typeof this.root.scrollTo === "function") {
      this.root.scrollTo({ top: top, behavior: behavior });
    } else {
      this.root.scrollTop = top;
    }
  };

  global.livecodeCodeView = {
    create: function (root) { return new CodeView(root); },
    normalizeText: normalizeText,
    splitLines: splitLines,
    parseSelection: parseSelection
  };
})(typeof window !== "undefined" ? window : this);

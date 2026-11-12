/*
 * A Prism grammar for Quarto (.qmd) and R Markdown (.Rmd) documents, from
 * markermd. It extends Prism's markdown grammar with:
 *  - executable chunks for any engine (```{r}, ```{python}, ```{bash}, ...),
 *    whose code is highlighted with the matching Prism language when that
 *    language is loaded and left plain otherwise;
 *  - knitr-style chunk headers (```{r label, fig.width=6, echo=FALSE});
 *  - Quarto #| chunk option lines, highlighted as YAML;
 *  - inline code, `r expr` (knitr) and `{r} expr` (Quarto);
 *  - Quarto div fences (::: {#id .class key="value"});
 *  - Pandoc attribute blocks on headings ({#id .class}).
 * YAML front matter and HTML comments come from the markdown and markup
 * grammars it extends. Registered as "qmd", with "rmd" as an alias.
 */
(function (Prism) {
  if (!Prism || !Prism.languages || !Prism.languages.markdown) {
    return;
  }

  // Chunk engine -> Prism language
  var ENGINES = {
    r: "r",
    python: "python",
    bash: "bash",
    sh: "bash",
    zsh: "bash",
    sql: "sql",
    julia: "julia",
    js: "javascript",
    javascript: "javascript",
    ojs: "javascript",
    css: "css",
    html: "markup",
    yaml: "yaml",
    json: "json",
    cpp: "cpp",
    rcpp: "cpp",
    c: "c",
    java: "java",
    go: "go",
    rust: "rust",
    tex: "latex",
    latex: "latex",
    md: "markdown",
    markdown: "markdown"
  };

  // A Pandoc attribute block: {#id .class key=value key="value"}
  var attrBlock = {
    pattern: /\{[^{}\n]*\}/,
    inside: {
      "string": /"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'/,
      "selector": /#[\w:.-]+/,
      "class-name": /\.[\w-]+/,
      "attr-name": /[\w.-]+(?=\s*=)/,
      "punctuation": /[{}=]/
    }
  };

  // A fence opener: ```{r label, fig.width=6, echo=FALSE}, ```{.python} or
  // a plain ```python fence
  function chunkHeader() {
    return {
      pattern: /^[ \t]*(?:`{3,}|~{3,})[^\n]*/,
      inside: {
        "string": /"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'/,
        "keyword": { pattern: /(^[ \t]*(?:`{3,}|~{3,})\{?\.?)[\w.-]+/, lookbehind: true },
        "attr-name": /[\w.-]+(?=\s*=)/,
        "boolean": /\b(?:TRUE|FALSE|T|F|true|false)\b/,
        "number": /\b\d+(?:\.\d+)?\b/,
        "punctuation": /`+|~+|[{},.]/,
        "operator": /=/
      }
    };
  }

  // #| option lines: the marker as a comment, the rest highlighted as YAML.
  // The YAML grammar sits under its own token so its comment rule cannot
  // swallow the whole line
  function chunkOption() {
    var inside = { "option-marker": { pattern: /^[ \t]*#\|/, alias: "comment" } };
    if (Prism.languages.yaml) {
      inside["option-body"] = { pattern: /[\s\S]+/, inside: Prism.languages.yaml };
    }
    return { pattern: /^[ \t]*#\|.*$/m, inside: inside };
  }

  // A chunk or fenced code block for one engine, whose body is highlighted
  // with `lang` (or left plain). Matches ```{engine ...}, ```{.engine} and a
  // plain ```engine fence; `braced` limits it to the {engine} forms
  function chunkToken(enginePattern, lang, braced) {
    var inside = {
      "chunk-header": chunkHeader(),
      "chunk-option": chunkOption(),
      "punctuation": /^[ \t]*(?:`{3,}|~{3,})[ \t]*$/m
    };
    var grammar = lang && Prism.languages[lang];
    inside["chunk-code"] = grammar ?
      { pattern: /[\s\S]+/, alias: "language-" + lang, inside: grammar } :
      { pattern: /[\s\S]+/ };
    var opener = "\\{\\.?(?:" + enginePattern + ")(?=[\\s,}])[^}\\n]*\\}";
    if (!braced) {
      opener = "(?:" + opener + "|(?:" + enginePattern + ")(?![\\w.-]))";
    }
    return {
      pattern: new RegExp(
        "^([ \\t]*)(`{3,}|~{3,})" + opener + "[^\\n]*\\n[\\s\\S]*?^[ \\t]*\\2[ \\t]*$",
        "m"
      ),
      lookbehind: true,
      greedy: true,
      alias: "chunk",
      inside: inside
    };
  }

  var qmd = Prism.languages.extend("markdown", {});

  // Every known engine gets its own token (Prism grammars are static, so the
  // body's language is fixed per token); any other {engine} is a plain chunk,
  // and any other plain fence stays markdown's code block
  var chunks = {};
  Object.keys(ENGINES).forEach(function (engine) {
    chunks["chunk-" + engine] = chunkToken(engine.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), ENGINES[engine], false);
  });
  chunks["chunk"] = chunkToken("[\\w.-]+", null, true);

  // Inline code run by an engine: `r expr` (knitr) or `{r} expr` (Quarto).
  // The delimiters and the engine have their own token names, and the code
  // its own nested token, so the R grammar's punctuation rule cannot replace
  // them
  chunks["inline-r"] = {
    pattern: /`(?:r|\{r\}) [^`\n]+`/,
    greedy: true,
    inside: {
      "inline-delimiter": { pattern: /^`|`$/, alias: "punctuation" },
      "inline-engine": { pattern: /^(?:\{r\}|r)(?= )/, alias: "keyword" },
      "inline-code": { pattern: /[\s\S]+/, alias: "language-r", inside: Prism.languages.r || {} }
    }
  };
  chunks["inline-code-engine"] = {
    pattern: /`\{[\w.-]+\} [^`\n]+`/,
    greedy: true,
    inside: {
      "inline-delimiter": { pattern: /^`|`$/, alias: "punctuation" },
      "inline-engine": { pattern: /^\{[\w.-]+\}/, alias: "keyword" }
    }
  };
  chunks["div-fence"] = {
    pattern: /^[ \t]*:{3,}.*$/m,
    inside: {
      "attr-block": attrBlock,
      "punctuation": /:{3,}/,
      "class-name": /[\w-]+/
    }
  };

  Prism.languages.qmd = qmd;
  Prism.languages.insertBefore("qmd", "code", chunks);
  qmd = Prism.languages.qmd;

  // Heading attribute blocks: ## Title {#id .class}
  var titles = Array.isArray(qmd.title) ? qmd.title : [qmd.title];
  titles.forEach(function (title) {
    var inside = title.inside || {};
    var rebuilt = { "attr-block": attrBlock };
    Object.keys(inside).forEach(function (key) { rebuilt[key] = inside[key]; });
    title.inside = rebuilt;
  });

  Prism.languages.qmd = qmd;
  Prism.languages.rmd = qmd;
})(typeof Prism !== "undefined" ? Prism : undefined);

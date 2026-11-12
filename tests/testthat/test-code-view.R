prism_dir = function() {
  pkg_resource("resources")
}

# A V8 context with Prism, every vendored component, the qmd grammar and the
# code view helpers loaded in the page's own order.
prism_context = function() {
  ctx = V8::v8()
  ctx$eval("var window = this;")
  for (script in prism_scripts())
    ctx$source(file.path(prism_dir(), script))
  ctx
}

highlight = function(ctx, text, language = "qmd") {
  ctx$assign("src", text)
  ctx$assign("lang", language)
  ctx$get("Prism.highlight(src, Prism.languages[lang], lang)")
}

test_that("the vendored Prism files are all present", {
  root = prism_dir()
  expect_true(all(file.exists(file.path(root, prism_scripts()))))
  expect_true(file.exists(file.path(root, "prism", "livecode-code-view.css")))
  expect_true(file.exists(file.path(root, "prism", "LICENSE")))
  expect_equal(
    sort(list.files(file.path(root, "prism", "components"))),
    sort(paste0("prism-", prism_components(), ".min.js"))
  )
})

test_that("file languages map to loaded Prism components", {
  files = c(
    "a.r", "a.R", "a.py", "a.qmd", "a.Rmd", "a.md", "a.sh", "a.sql", "a.json", "a.yml",
    "a.toml", "a.ini", "a.js", "a.ts", "a.css", "a.html", "a.xml", "a.c", "a.cpp",
    "a.java", "a.jl", "a.rs", "a.go", "a.tex", "Dockerfile", "Makefile"
  )
  langs = vapply(files, file_code_language, character(1))
  expect_true(all(langs %in% c(prism_components(), "qmd")))

  expect_equal(unname(langs[c("a.r", "a.R", "a.py", "a.qmd", "a.Rmd")]), c("r", "r", "python", "qmd", "qmd"))
  expect_equal(file_code_language("/some/dir.with.dots/script.sql"), "sql")
  expect_equal(file_code_language("dir/Dockerfile"), "docker")
  expect_equal(file_code_language("notes.txt"), "none")
  expect_equal(file_code_language("README"), "none")
  expect_equal(file_code_language("archive.tar.gz"), "none")
})

test_that("every language the page can ask for has a grammar", {
  skip_if_not_installed("V8")
  ctx = prism_context()
  for (lang in c(prism_components(), "qmd", "rmd")) {
    ctx$assign("lang", lang)
    expect_true(ctx$get("typeof Prism.languages[lang] === 'object'"), info = lang)
  }
  expect_true(ctx$get("Prism.manual"))
})

test_that("Prism encodes the source text", {
  skip_if_not_installed("V8")
  ctx = prism_context()
  html = highlight(ctx, "x <- \"&amp; &lt;-\"", "r")
  expect_match(html, "&amp;amp; &amp;lt;-", fixed = TRUE)
  expect_false(grepl("<-", html, fixed = TRUE))
})

test_that("the qmd grammar highlights chunks, options, inline code and Quarto syntax", {
  skip_if_not_installed("V8")
  ctx = prism_context()

  doc = paste(c(
    "---",
    "title: \"HW\"",
    "---",
    "",
    "```{r setup, include=FALSE, fig.cap=\"A, b\"}",
    "library(dplyr)",
    "```",
    "",
    "## Question 1 {#q1 .unnumbered}",
    "",
    "<!-- placeholder -->",
    "",
    "Rows: `r nrow(df)`, cols: `{r} ncol(df)`, py: `{python} 1 + 1`.",
    "",
    "::: {#answer .callout-note}",
    "Text.",
    ":::",
    "",
    "```{python}",
    "#| label: py",
    "#| echo: true",
    "import numpy as np",
    "```",
    "",
    "```{stan}",
    "model { }",
    "```",
    "",
    "```r",
    "plain(1)",
    "```"
  ), collapse = "\n")
  html = highlight(ctx, doc)

  has = function(pattern) grepl(pattern, html, fixed = TRUE)
  expect_true(has("front-matter-block"))
  expect_true(has("<span class=\"token comment\">&lt;!-- placeholder --></span>"))
  expect_true(has("token chunk-r chunk"))
  expect_true(has("<span class=\"token keyword\">r</span> setup"))
  expect_true(has("<span class=\"token attr-name\">include</span>"))
  expect_true(has("<span class=\"token boolean\">FALSE</span>"))
  expect_true(has("<span class=\"token string\">\"A, b\"</span>"))
  expect_true(has("token chunk-code language-r"))
  expect_true(has("token chunk-python chunk"))
  expect_true(has("<span class=\"token option-marker comment\">#|</span>"))
  expect_true(has("<span class=\"token key atrule\">label</span>"))
  expect_true(has("<span class=\"token keyword\">import</span>"))
  expect_true(has("<span class=\"token inline-engine keyword\">r</span>"))
  expect_true(has("<span class=\"token inline-engine keyword\">{r}</span>"))
  expect_true(has("<span class=\"token inline-engine keyword\">{python}</span>"))
  expect_true(has("token inline-code language-r"))
  expect_true(has("token div-fence"))
  expect_true(has("<span class=\"token selector\">#answer</span>"))
  expect_true(has("<span class=\"token class-name\">.callout-note</span>"))
  expect_true(has("<span class=\"token selector\">#q1</span>"))
  expect_true(has("<span class=\"token keyword\">stan</span>"))
  expect_match(html, "plain<span class=\"token punctuation\">\\(</span><span class=\"token number\">1</span>")

  expect_identical(highlight(ctx, doc, "rmd"), html)
})

test_that("highlighted code splits into one valid line per source line", {
  skip_if_not_installed("V8")
  ctx = prism_context()

  src = paste(c("", "", "function f(x) {", "  /* a <b> &", "  comment */", "  return x;", "}"), collapse = "\n")
  ctx$assign("html", highlight(ctx, src, "javascript"))
  lines = ctx$get("livecodeCodeView.splitLines(html)")
  expect_length(lines, 7)
  expect_equal(lines[1:2], c("", ""))

  # Each line is balanced and, without its tags, is the source line.
  opens = lengths(regmatches(lines, gregexpr("<span", lines, fixed = TRUE)))
  closes = lengths(regmatches(lines, gregexpr("</span>", lines, fixed = TRUE)))
  expect_equal(opens, closes)
  unescape = function(x) gsub("&amp;", "&", gsub("&lt;", "<", x, fixed = TRUE), fixed = TRUE)
  expect_equal(unescape(gsub("<[^>]+>", "", lines)), strsplit(src, "\n")[[1]])
  # The comment's span is closed on line 4 and reopened on line 5.
  expect_match(lines[4], "<span class=\"token comment\">/\\* a &lt;b> &amp;</span>$")
  expect_match(lines[5], "^<span class=\"token comment\">  comment \\*/</span>$")
})

test_that("text normalization and selection parsing", {
  skip_if_not_installed("V8")
  ctx = prism_context()

  norm = function(x) {
    ctx$assign("x", x)
    ctx$get("livecodeCodeView.normalizeText(x)")
  }
  expect_equal(norm("a\nb\n"), "a\nb")
  expect_equal(norm("a\r\nb\r\n"), "a\nb")
  expect_equal(norm("\n\na\n\n"), "\n\na\n")
  expect_equal(norm(""), "")

  sel = function(x) {
    ctx$assign("x", x)
    ctx$get("livecodeCodeView.parseSelection(x)", simplifyVector = FALSE)
  }
  expect_equal(sel(""), list())
  expect_equal(sel("3"), list(list(3L, 3L)))
  expect_equal(sel("1-10,12"), list(list(1L, 10L), list(12L, 12L)))
  expect_equal(sel("5-2,x,0,7"), list(list(2L, 5L), list(7L, 7L)))
})

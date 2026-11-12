page_refs = function(page) {
  refs = regmatches(page, gregexpr("(src|href)=\"[^\"]*\"", page))[[1]]
  sub("^(src|href)=\"([^\"]*)\"$", "\\2", refs)
}

test_that("the page references only files that exist under inst/resources", {
  page = livecode_page("lecture.R")
  refs = page_refs(page)

  expect_gt(length(refs), length(prism_scripts()))
  expect_true(all(startsWith(refs, "web/")))
  expect_false(any(grepl("^(https?:)?//", refs)))

  files = file.path(pkg_resource("resources"), sub("^web/", "", refs))
  expect_true(all(file.exists(files)), info = paste(refs[!file.exists(files)], collapse = ", "))

  # Every component is loaded, in dependency order.
  scripts = refs[endsWith(refs, ".js")]
  expect_equal(scripts[scripts %in% paste0("web/", prism_scripts())], paste0("web/", prism_scripts()))
})

test_that("the page carries the language, viewport and accessibility markup", {
  page = livecode_page("a<b>.qmd")
  expect_match(page, "<html lang=\"en\">", fixed = TRUE)
  expect_match(page, "<meta name=\"viewport\"", fixed = TRUE)
  expect_match(page, "<title>a&lt;b&gt;.qmd</title>", fixed = TRUE)
  expect_match(page, "data-language=\"qmd\"", fixed = TRUE)
  expect_match(page, "<div id=\"progressbar\" aria-hidden=\"true\">", fixed = TRUE)
  expect_match(page, "role=\"status\"", fixed = TRUE)
  # Whitespace between <pre> and <code> would show as a blank first line.
  expect_match(page, "<pre><code></code></pre>", fixed = TRUE)

  expect_match(livecode_page("notes.txt"), "data-language=\"none\"", fixed = TRUE)
})

test_that("the connect frame carries the file's language", {
  dir = withr::local_tempdir()
  f = file.path(dir, "doc.qmd")
  writeLines("# Title", f)
  srv = file_stream_server("127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, interval = 10)
  on.exit(srv$stop())

  frame = srv$connect_frame()
  expect_equal(frame$lang, "qmd")
  expect_null(srv$build_frame()$lang)
})

test_that("the page carries the viewer's display settings", {
  page = livecode_page("lecture.R")
  expect_match(page, "<script src=\"web/livecode/livecode-settings.js\"></script>\n    <script src=\"web/livecode/livecode.js\"></script>", fixed = TRUE)
  expect_match(page, "<div id=\"livecode-settings\" class=\"livecode-settings\" hidden>", fixed = TRUE)
  expect_match(page, "<button type=\"button\" id=\"livecode-settings-toggle\"[^>]*aria-expanded=\"false\"[^>]*aria-controls=\"livecode-settings-panel\"[^>]*aria-label=\"Display settings\"")
  expect_match(page, "id=\"livecode-settings-panel\"[^>]* hidden>")
  for (kind in c("-1", "1", "reset"))
    expect_match(page, paste0("<button type=\"button\" data-size=\"", kind, "\" aria-label=\"[^\"]+\">"))
  expect_match(page, "<output id=\"livecode-size-value\"", fixed = TRUE)
  expect_match(page, "<label><input type=\"checkbox\" id=\"livecode-follow\" checked> Follow cursor</label>", fixed = TRUE)
  expect_match(page, "<label><input type=\"checkbox\" id=\"livecode-highlight\" checked> Highlight lines</label>", fixed = TRUE)
})

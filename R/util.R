get_browser = function() {
  getOption("browser")
}

pkg_resource = function(..., must_work = FALSE) {
  system.file(..., package = "livecode", mustWork = must_work)
}

html_escape = function(x) {
  x = gsub("&", "&amp;", x, fixed = TRUE)
  x = gsub("<", "&lt;", x, fixed = TRUE)
  x = gsub(">", "&gt;", x, fixed = TRUE)
  x = gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

cli_escape = function(x) {
  x = gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

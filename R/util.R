is_rstudio = function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

using_project = function() {
  if (is_rstudio())
    !is.null(rstudioapi::getActiveProject())
  else
    FALSE
}

# The RStudio viewer sandboxes websockets, so always use the system browser.
get_browser = function() {
  getOption("browser")
}

pkg_resource = function(..., must_work = FALSE) {
  system.file(..., package = "livecode", mustWork = must_work)
}

get_template = function(name) {
  if (file.exists(name))
    return(name)

  file = pkg_resource("templates", paste0(name, ".html"))

  if (file == "")
    cli::cli_abort("Template {.val {name}} does not exist.")

  file
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

is_rstudio = function () {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

get_browser = function() {
  if (is_rstudio())
    rstudioapi::viewer
  else
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
    usethis::ui_stop("Template {usethis::ui_value(name)} does not exist.")

  file
}

is_rstudio = function () {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

get_browser = function() {
  #if (is_rstudio())
  #  rstudioapi::viewer
  #else
  #  getOption("browser")

  # Use system browser until sandboxing is fixed
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

is_error = function(obj) {
  inherits(obj, "try-error")
}

port_available = function(host, port) {
  ll = httpuv:::logLevel("OFF")
  s = try({httpuv::startServer(host, port, list())}, silent = TRUE)
  httpuv:::logLevel(ll)

  if (is_error(s)) {
    FALSE
  } else {
    httpuv::stopServer(s)
    TRUE
  }
}

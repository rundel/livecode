is_rstudio = function () {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

get_browser = function() {
  if (is_rstudio())
    rstudioapi::viewer
  else
    getOption("browser")
}

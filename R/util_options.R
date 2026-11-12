get_option = function(x, default = NULL) {
  if (!is_rstudio()) {
    return(default)
  }

  rstudio_env = tryCatch(as.environment("tools:rstudio"), error = function(e) NULL)
  if (is.null(rstudio_env)) {
    return(default)
  }

  read_ui_pref = get0(".rs.readUiPref", envir = rstudio_env, mode = "function")
  if (is.null(read_ui_pref)) {
    return(default)
  }

  value = read_ui_pref(x)
  if (length(value) == 0)
    default
  else
    value
}

check_strip_trailing_ws = function() {
  isTRUE(get_option("strip_trailing_whitespace", default = FALSE))
}

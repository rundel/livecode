#' @export

bitly_get_token = function() {
  token = Sys.getenv("BITLY_PAT", "")
  if (token != "")
    return(invisible(token))

  if (file.exists("~/.bitly/token")) {
    bitly_set_token("~/.bitly/token")
    return(invisible(bitly_get_token()))
  }

  usethis::ui_stop( paste0(
    "Unable to locate bitly token, please use {usethis::ui_code('bitly_set_token')}",
    " or define the BITLY_PAT environmental variable."
  ) )
}

#' @export

bitly_set_token = function(token) {
  token = as.character(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  Sys.setenv(BITLY_PAT = token)
}

#' @export

bitly_reset_token = function() {
  Sys.unsetenv("BITLY_PAT")
}


#' @export
bitly_test_token = function(token = bitly_get_token()) {
  res = purrr::safely(bitly_api_user)()

  status_msg(
    res,
    "Your bitly token is functioning correctly.",
    "Your bitly token failed to authenticate.",
  )
}

bitly_available = function() {
  res = purrr::safely(bitly_api_user)()
  succeeded(res)
}

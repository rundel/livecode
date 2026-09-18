#' Retrieve the active Bitly token
#'
#' @description
#' Retrieves the Bitly personal access token used for authentication with the
#' Bitly API.
#'
#' The token is searched for in the following order:
#' \enumerate{
#'   \item The \code{BITLY_PAT} environment variable.
#'   \item A local file at \code{~/.bitly/token}.
#' }
#'
#' If a token file is found, it is loaded automatically using
#' \code{bitly_set_token()}.
#'
#' @return
#' An invisible character string containing the active Bitly token.
#'
#' @details
#' If no token can be located, an error is raised instructing the user to set
#' a token manually.
#'
#' @examples
#' \dontrun{
#' bitly_get_token()
#' }
#'
#' @seealso
#' \code{\link{bitly_set_token}},
#' \code{\link{bitly_reset_token}},
#' \code{\link{bitly_test_token}}
#'
#' @export
bitly_get_token = function() {
  token = Sys.getenv("BITLY_PAT", "")
  if (token != "")
    return(invisible(token))
  
  if (file.exists("~/.bitly/token")) {
    bitly_set_token("~/.bitly/token")
    return(invisible(bitly_get_token()))
  }
  
  usethis::ui_stop(paste0(
    "Unable to locate bitly token, please use {usethis::ui_code('bitly_set_token')}",
    " or define the BITLY_PAT environmental variable."
  ))
}



#' Set the active Bitly token
#'
#' @description
#' Sets the Bitly personal access token used for authentication with the Bitly
#' API by storing it in the current R session environment variable
#' \code{BITLY_PAT}.
#'
#' The token may be supplied directly as a character string or indirectly as a
#' path to a file containing the token.
#'
#' @param token Character string giving the Bitly token, or a path to a file
#' containing the token.
#'
#' @return
#' Invisibly returns \code{NULL}. Called for side effects.
#'
#' @details
#' If \code{token} is a valid file path, the first line(s) of the file are read
#' and used as the token value.
#'
#' This affects only the current R session unless the user also stores the token
#' in a startup file such as \code{~/.Renviron}.
#'
#' @examples
#' \dontrun{
#' # Set directly
#' bitly_set_token("your_token_here")
#'
#' # Set from file
#' bitly_set_token("~/.bitly/token")
#' }
#'
#' @seealso
#' \code{\link{bitly_get_token}},
#' \code{\link{bitly_reset_token}}
#'
#' @export
bitly_set_token = function(token) {
  token = as.character(token)
  
  if (file.exists(token))
    token = readLines(token, warn = FALSE)
  
  Sys.setenv(BITLY_PAT = token)
}



#' Remove the active Bitly token
#'
#' @description
#' Removes the \code{BITLY_PAT} environment variable from the current R session,
#' effectively clearing the active Bitly token.
#'
#' @return
#' Invisibly returns \code{NULL}. Called for side effects.
#'
#' @examples
#' \dontrun{
#' bitly_reset_token()
#' }
#'
#' @seealso
#' \code{\link{bitly_set_token}},
#' \code{\link{bitly_get_token}}
#'
#' @export
bitly_reset_token = function() {
  Sys.unsetenv("BITLY_PAT")
}



#' Test Bitly authentication
#'
#' @description
#' Verifies that the current Bitly personal access token can successfully
#' authenticate with the Bitly API.
#'
#' By default, the token is obtained from \code{bitly_get_token()}.
#'
#' @param token Character string giving a Bitly token to test. Defaults to the
#' active token returned by \code{bitly_get_token()}.
#'
#' @return
#' Returns the result of \code{status_msg()}, typically a logical or invisible
#' status indicator depending on implementation.
#'
#' @details
#' A success or failure message is displayed indicating whether authentication
#' succeeded.
#'
#' @examples
#' \dontrun{
#' bitly_test_token()
#'
#' bitly_test_token("your_token_here")
#' }
#'
#' @seealso
#' \code{\link{bitly_get_token}},
#' \code{\link{bitly_set_token}}
#'
#' @export
bitly_test_token = function(token = bitly_get_token()) {
  res = purrr::safely(bitly_api_user)()
  
  status_msg(
    res,
    "Your bitly token is functioning correctly.",
    "Your bitly token failed to authenticate."
  )
}
bitly_available = function() {
  res = purrr::safely(bitly_api_user)()
  succeeded(res)
}

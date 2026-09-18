.globals = new.env()

.globals$servers = list()

register_server = function(server) {
  match = purrr::map_lgl(.globals$servers, identical, y = server)

  if (any(match))
    usethis::ui_warn("Server is already registered.")
  else
    .globals$servers[[length(.globals$servers)+1]] = server

  invisible()
}

deregister_server = function(server) {
  match = purrr::map_lgl(.globals$servers, identical, y = server)

  if (!any(match))
    usethis::ui_warn("Unable to find matching running server.")
  else
    .globals$servers = .globals$servers[!match]

  invisible()
}

#' Stop all active livecode servers
#'
#' @description
#' Stops all currently registered \code{livecode} server instances.
#'
#' This is a convenience function for shutting down every active server created
#' during the current R session, including any associated background tunnels
#' (for example, ngrok tunnels started through \code{serve_file()}).
#'
#' @details
#' The function iterates over all server objects stored in the internal
#' server registry and calls \code{$stop()} on each one.
#'
#' This is useful when multiple livecode sessions have been started and you want
#' to cleanly terminate all of them at once.
#'
#' @return
#' Invisibly returns \code{NULL}. Called for side effects.
#'
#' @examples
#' \dontrun{
#' # Start multiple servers
#' serve_file("script1.R")
#' serve_file("script2.R")
#'
#' # Stop them all
#' stop_all()
#' }
#'
#' @seealso
#' \code{\link{serve_file}},
#' \code{\link{list_servers}}
#'
#' @export
stop_all = function() {
  purrr::walk(.globals$servers, ~ .$stop())
}



#' List active livecode servers
#'
#' @description
#' Returns the currently registered \code{livecode} server objects active in the
#' current R session.
#'
#' Each element is a \code{LiveCodeServer_Interface} R6 object representing a
#' running (or previously started) server instance.
#'
#' @details
#' This function is useful for inspecting currently active servers, manually
#' interacting with specific server objects, or debugging multiple concurrent
#' sessions.
#'
#' @return
#' A list of \code{LiveCodeServer_Interface} objects.
#'
#' @examples
#' \dontrun{
#' # Start a server
#' srv <- serve_file("script.R")
#'
#' # View active servers
#' list_servers()
#' }
#'
#' @seealso
#' \code{\link{serve_file}},
#' \code{\link{stop_all}}
#'
#' @export
list_servers = function() {
  .globals$servers
}



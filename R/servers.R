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

#' @export
stop_all = function() {
  purrr::walk(.globals$servers, ~ .$stop())
}

#' @export
list_servers = function() {
  .globals$servers
}




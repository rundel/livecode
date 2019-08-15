file_stream_server = function(host, port, file, interval = 3, template = "prism") {
  port = as.integer(port)
  file_cache = file_cache(file)
  page = glue::glue(
    readr::read_file(get_template(template)),
    lang = "r",
    title = file
  )

  get_next_ws_id = local({
    next_ws_id = 0L
    function() {
      sprintf("%012d", next_ws_id <<- next_ws_id + 1L)
    }
  })

  websockets = new.env(parent = emptyenv())

  websocket_loop = function() {
    if (!server$isRunning())
      return()

    message(Sys.time())
    rlang::env_print(websockets)
    msg = list(interval = interval)
    if (file_cache$need_update())
      msg$content = file_cache$content

    msg = jsonlite::toJSON(msg, auto_unbox = TRUE)

    for(ws_id in names(websockets)) {
      websockets[[ws_id]]$send(msg)
    }

    later::later(websocket_loop, interval)
  }

  app = list(
    call = function(req) {
      list(
        status = 200L,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = page
      )
    },

    onWSOpen = function(ws) {
      ws_id = get_next_ws_id()
      websockets[[ws_id]] = ws

      ws$onClose(
        function() {
          rm(list = ws_id, envir = websockets)
        }
      )

      ## Send initial message with current file contents
      msg = list(
        interval = interval,
        content = file_cache$content
      )
      ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))

      if (as.integer(ws_id) == 1)
        websocket_loop()
    },

    staticPaths = list(
      "/web" = livecode:::pkg_resource("resources")
    )
  )

  server = httpuv::startServer(host, port, app)
}


#' @export
serve_file = function() {
  later::later(~browseURL("http://localhost:5000/", browser = get_browser()), 1)
  file_stream_server("0.0.0.0", 5000L, "reprex.R", template="prism")
}



livecode_server = R6::R6Class(
  "livecode_server",
  public = list(
    initialize = function(ip, port, file, file_id, interval = 1, template = "prism") {
      private$ip = ip
      private$port = as.integer(port)
      private$file_cache = file_cache(file, file_id)
      private$interval = interval
      private$template = template

      private$page = glue::glue(
        readr::read_file(get_template(template)),
        lang = "r",
        title = file
      )

      private$server = private$start_server()

      addr = paste(ip, port, sep=":")
      usethis::ui_done( paste(
        "Started sharing {usethis::ui_value(fs::path_file(file))} at {usethis::ui_value(addr)}."
      ) )
    }
  ),
  private = list(
    ip = NULL,
    port = NULL,
    file_cache = NULL,
    page = NULL,

    interval = NULL,
    template = NULL,

    server = NULL,

    msg_queue = list(),

    start_server = function() {

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

        #message(Sys.time())

        if (is_rstudio() & !is.null(file_id)) {
          rstudioapi::documentSave(file_id)
        }

        msg = list(interval = interval)
        if (file_cache$need_update())
          msg[["content"]] = file_cache$content

        if (is_rstudio()) {
          ctx = rstudioapi::getSourceEditorContext()
          open_file = path.expand(ctx[["path"]])

          if (file == open_file) {
            ln = extract_line_nums(ctx[["selection"]])
            msg[["selection"]] = ln
          }
        }

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

      httpuv::startServer(private$ip, private$port, app)
    }
  ),
  active = list(

  )
)



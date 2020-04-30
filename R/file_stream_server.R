# Low level web server class that adds needed message queue methods
msg_server <- R6::R6Class(
  "msg_server",
  cloneable = FALSE,
  
  inherit = httpuv:::WebServer,
  
  public = list(
    have_msgs = function() {
      length(private$msg_queue) > 0
    },

    get_msgs = function() {
      msgs = private$msg_queue
      private$msg_queue = list()
      msgs
    },

    peek_msgs = function() {
      purrr::map_chr(private$msg_queue, ~ .$get_text())
    },

    add_msg = function(m) {


      if (is.character(m) & length(m) == 1) {
        m = noty_msg$new(m)
      }

      if (!inherits(m, "noty_msg")) {
        usethis::ui_stop("Invalid message type, must either be a character or noty_msg object.")
      }
      private$msg_queue = append(private$msg_queue, m)
    }
  ),
  private = list(
    msg_queue = list()
  )
)


file_stream_server = function(host, port, file_cache, interval, template = "prism") {
  port = as.integer(port)
  page = glue::glue(
    readr::read_file(get_template(template)),
    lang = "r",
    title = "livecode"
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
    
    file_cache$save()

    msg = list(interval = interval)
    if (file_cache$file_changed())
      msg[["content"]] = file_cache$get_content()

    if (server$have_msgs())
      msg[["messages"]] = purrr::map(server$get_msgs(), ~ .$get_msg())

    if (is_rstudio()) {
      ctx = rstudioapi::getSourceEditorContext()
      open_file = path.expand(ctx[["path"]])

      if (file_cache$get_path() == open_file) {
        msg[["selection"]] = extract_line_nums(ctx[["selection"]])
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
          'Content-Type'='text/html; charset=UTF-8'
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
        content = file_cache$get_content()
      )
      ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))

      if (as.integer(ws_id) == 1)
        websocket_loop()
    },

    staticPaths = list(
      "/web" = livecode:::pkg_resource("resources")
    )
  )

  # Must be assigned to a variable for the websocket_loop above to work
  server = msg_server$new(host, port, app)

  server
}


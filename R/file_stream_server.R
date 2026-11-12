lc_server = R6::R6Class(
  "LiveCodeServer",
  cloneable = FALSE,
  public = list(
    initialize = function(host, port, page, file, file_id = NULL, interval = 1,
                          lang = file_code_language(file), auto_save = FALSE,
                          editor = editor_backend()) {
      private$page = page
      private$file = file
      private$lang = lang
      private$file_id = file_id
      private$interval = interval
      private$auto_save = isTRUE(auto_save)
      private$editor = editor
      private$cache = file_cache(file)
      private$state = list(content = private$cache$content, selection = NULL)
      private$websockets = new.env(parent = emptyenv())

      private$handle = httpuv::startServer(host, port, private$app())

      later::later(function() self$tick(), 0)
    },

    is_running = function() {
      !is.null(private$handle) && private$handle$isRunning()
    },

    stop = function() {
      if (self$is_running())
        httpuv::stopServer(private$handle)
      invisible(self)
    },

    n_clients = function() {
      length(ls(private$websockets))
    },

    have_msgs = function() {
      length(private$msg_queue) > 0
    },

    get_msgs = function() {
      msgs = private$msg_queue
      private$msg_queue = list()
      msgs
    },

    add_msg = function(m) {
      m = as_noty_msg(m)
      private$msg_queue = append(private$msg_queue, m)
      invisible(self)
    },

    # A sticky message goes to the clients connected now and to every client
    # that connects later, so it is never queued.
    set_sticky_msg = function(m, announce = TRUE) {
      private$sticky_msg = if (is.null(m)) NULL else as_noty_msg(m)

      if (announce && !is.null(private$sticky_msg)) {
        self$broadcast(list(
          interval = private$interval,
          messages = list(private$sticky_msg$get_msg())
        ))
      }

      invisible(self)
    },

    # Everything a newly connected client needs to match the clients that
    # are already connected.
    connect_frame = function() {
      frame = list(
        interval = private$interval,
        lang = private$lang,
        content = private$state[["content"]]
      )

      if (!is.null(private$state[["selection"]]))
        frame[["selection"]] = private$state[["selection"]]

      if (!is.null(private$sticky_msg))
        frame[["messages"]] = list(private$sticky_msg$get_msg())

      frame
    },

    # Builds the next update and records it as the broadcast state. The
    # cache is compared with what was last broadcast rather than with its
    # own previous read, so a refresh triggered elsewhere cannot hide a
    # change from clients.
    build_frame = function(ctx = editor_context(private$editor)) {
      frame = list(interval = private$interval)

      content = private$cache$content
      if (!identical(content, private$state[["content"]])) {
        frame[["content"]] = content
        private$state[["content"]] = content
      }

      selection = editor_selection(private$file, ctx, private$editor)
      if (!is.null(selection) && !identical(selection, private$state[["selection"]])) {
        frame[["selection"]] = selection
        private$state[["selection"]] = selection
      }

      # Queued messages wait until someone is connected to see them.
      if (self$n_clients() > 0 && self$have_msgs()) {
        frame[["messages"]] = lapply(self$get_msgs(), function(m) m$get_msg())
      }

      frame
    },

    broadcast = function(frame) {
      if (self$n_clients() == 0)
        return(invisible(self))

      json = frame_to_json(frame)
      for (ws_id in ls(private$websockets)) {
        ws = private$websockets[[ws_id]]
        if (!is.null(ws))
          try(ws$send(json), silent = TRUE)
      }

      invisible(self)
    },

    tick = function() {
      if (!self$is_running())
        return(invisible(FALSE))

      on.exit(
        later::later(function() self$tick(), private$interval),
        add = TRUE
      )

      ctx = editor_context(private$editor)

      if (private$auto_save) {
        private$guard("Auto-save", function() {
          editor_save(private$file, private$file_id, ctx, private$editor)
        })
      }
      private$guard("Broadcast update", function() {
        frame = self$build_frame(ctx)
        self$broadcast(frame)
      })

      invisible(TRUE)
    }
  ),

  private = list(
    handle = NULL,
    page = NULL,
    file = NULL,
    file_id = NULL,
    lang = NULL,
    interval = NULL,
    auto_save = FALSE,
    editor = "none",
    cache = NULL,
    state = NULL,
    websockets = NULL,
    next_ws_id = 0L,
    msg_queue = list(),
    sticky_msg = NULL,
    errors = list(),

    # Reports a failing step once, until it succeeds again, instead of on
    # every tick.
    guard = function(label, f) {
      tryCatch(
        {
          f()
          private$errors[[label]] = NULL
        },
        error = function(e) {
          msg = conditionMessage(e)
          if (!identical(private$errors[[label]], msg)) {
            private$errors[[label]] = msg
            cli::cli_alert_danger(
              "livecode: {label} failed, will keep retrying: {msg}"
            )
          }
        }
      )
    },

    app = function() {
      list(
        call = function(req) {
          list(
            status = 200L,
            headers = list(
              "Content-Type" = "text/html; charset=UTF-8"
            ),
            body = private$page
          )
        },

        onWSOpen = function(ws) {
          private$next_ws_id = private$next_ws_id + 1L
          ws_id = paste0("ws", private$next_ws_id)
          private$websockets[[ws_id]] = ws

          ws$onClose(function() {
            if (exists(ws_id, envir = private$websockets, inherits = FALSE))
              rm(list = ws_id, envir = private$websockets)
          })

          ws$send(frame_to_json(self$connect_frame()))
        },

        staticPaths = list(
          "/web" = pkg_resource("resources")
        )
      )
    }
  )
)

as_noty_msg = function(m) {
  if (is.character(m) && length(m) == 1)
    m = noty_msg$new(m)

  if (!inherits(m, "noty_msg"))
    cli::cli_abort("Invalid message type, must either be a character or {.cls noty_msg} object.")

  m
}

frame_to_json = function(frame) {
  jsonlite::toJSON(frame, auto_unbox = TRUE)
}

#' Create a low-level livecode file streaming server
#'
#' @description
#' Creates and returns a low-level HTTP/WebSocket server used by
#' \pkg{livecode} to broadcast the contents of a source file to connected
#' browsers.
#'
#' The server hosts an HTML page, serves supporting static assets, and maintains
#' WebSocket connections to all connected clients. File changes, editor cursor
#' selections, and queued messages are pushed to clients at a fixed interval.
#'
#' This function is primarily an internal constructor used by
#' \code{\link{serve_file}} and \code{LiveCodeServer_Interface}.
#'
#' @param host Character string giving the IP address on which the
#'   server should listen.
#' @param port Integer port number for the server.
#' @param file Path to the file being broadcast.
#' @param file_id Optional RStudio document id of the file, used to save it
#'   and to notice when it is renamed. Positron has no document ids.
#' @param interval Numeric update interval in seconds between broadcast ticks.
#' @param auto_save Logical. Should the file be saved in the editor on each
#'   tick.
#' @param editor The editor backend, one of \code{"rstudio"},
#'   \code{"positron"}, or \code{"none"}. Detected by default, see
#'   \code{\link{serve_file}}.
#'
#' @details
#' The server performs the following tasks:
#'
#' \itemize{
#'   \item Serves an HTML page that highlights the file with Prism in the
#'     language given by its extension (see \code{file_code_language()}).
#'   \item Tracks the source file using \code{\link{file_cache}}.
#'   \item Pushes updated file contents to clients when the file changes.
#'   \item Pushes queued messages from the server message queue.
#'   \item In RStudio or Positron, optionally saves the file on each update
#'     tick. Positron can only save the active editor, so there the file is
#'     saved only while it is the active document.
#'   \item In RStudio or Positron, broadcasts the current editor selection
#'     when the served file is the active document, and an empty selection
#'     when it is not.
#'   \item In RStudio, notices when the document is renamed or saved under
#'     another name, tells the instructor once, and stops auto-saving it.
#'   \item Serves static assets (JavaScript, CSS, etc.) from the package
#'     resources directory under \code{/web}.
#' }
#'
#' The update loop starts with the server and keeps running until the server
#' is stopped. Errors in a tick (for example a briefly missing file or a
#' failing editor call) are reported once and do not stop the loop.
#'
#' Connected clients receive JSON messages over WebSocket containing one or more
#' of:
#'
#' \itemize{
#'   \item \code{content}: updated file contents
#'   \item \code{messages}: queued notifications
#'   \item \code{selection}: highlighted line selection, such as
#'     \code{"3-5,9"}, or \code{""} to clear it
#'   \item \code{interval}: refresh interval
#'   \item \code{lang}: the Prism language of the file (connect frame only)
#' }
#'
#' A newly connected client receives the current broadcast state (content,
#' selection, and any sticky message) straight away.
#'
#' @return
#' A \code{LiveCodeServer} R6 object.
#'
#' @seealso
#' \code{\link{serve_file}},
#' \code{\link{file_cache}},
#' \code{\link{lc_server_iface}}
#'
#' @keywords internal
file_stream_server = function(host, port, file, file_id = NULL, interval = 1,
                              auto_save = FALSE, editor = editor_backend()) {
  port = as.integer(port)
  lang = file_code_language(file)
  page = livecode_page(file, lang)

  lc_server$new(
    host, port, page, file, file_id, interval, lang,
    auto_save = auto_save, editor = editor
  )
}

#' Livecode Server Interface
#'
#' @description
#' This is a high level, user facing interface class that allows
#' for the creation of a livecode server sharing a specific file.
#' The interface also provides additional tools for sending messages.
#'
#' @export
lc_server_iface = R6::R6Class(
  "LiveCodeServer_Interface",
  cloneable = FALSE,
  private = list(
    file = NULL,
    file_id = NULL,
    auto_save = FALSE,
    editor = "none",
    ip = NULL,
    port = NULL,
    interval = NULL,
    server = NULL,
    sticky_msg = NULL,

    init_file = function(file, auto_save) {
      editor = editor_backend()
      ctx = NULL

      if (!is.null(file)) {
        if (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file))
          cli::cli_abort("{.arg file} must be a single file path.")

        file = path.expand(file)
        if (!file.exists(file))
          cli::cli_abort("Unable to locate file {.file {file}}.")

        if (!identical(editor, "none"))
          ctx = editor_open(file, editor)
      } else {
        if (identical(editor, "none")) {
          cli::cli_abort(c(
            "No file specified.",
            "i" = "Outside RStudio and Positron, pass the path of the file to share as {.arg file}."
          ))
        }

        ctx = editor_context(editor)
        if (is.null(ctx)) {
          cli::cli_abort(c(
            "No file specified and no document is open in the editor.",
            "i" = "Open the file to share, or pass its path as {.arg file}."
          ))
        }
        if (is.null(ctx[["path"]])) {
          cli::cli_abort(c(
            "The active document has not been saved.",
            "i" = "Save it to a file first, or pass the path of the file to share as {.arg file}."
          ))
        }

        file = path.expand(ctx[["path"]])
        if (!file.exists(file))
          cli::cli_abort("Unable to locate file {.file {file}}.")
      }

      private$file = file
      private$file_id = ctx[["id"]]
      private$auto_save = isTRUE(auto_save)
      private$editor = editor
    },

    init_ip = function(ip) {
      if (is.null(ip)) {
        ip = network_interfaces()[["ip"]][1]
      }

      if (length(ip) != 1 || is.na(ip)) {
        cli::cli_abort("{.arg ip} must be a single IPv4 address.")
      }

      if (is_ipv6(ip)) {
        cli::cli_abort(c(
          "IPv6 addresses ({.val {ip}}) are not supported.",
          "i" = "Use an IPv4 address, see {.fn livecode::network_interfaces}."
        ))
      }

      if (!is_ipv4(ip)) {
        cli::cli_abort("Invalid ip address provided ({.val {ip}}).")
      }

      private$ip = ip
    },

    init_port = function(port) {
      if (is.null(port)) {
        port = httpuv::randomPort(host = private$ip)
      }

      port = suppressWarnings(as.integer(port))

      if (length(port) != 1 || is.na(port) || port < 1024L || port > 49151L) {
        cli::cli_abort(
          "Invalid port ({.val {port}}), value must be between 1024 and 49151."
        )
      }

      private$port = port
    },

    init_interval = function(interval) {
      if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) || interval <= 0) {
        cli::cli_abort("{.arg interval} must be a single positive number of seconds.")
      }

      private$interval = interval
    },

    init_auto_save = function() {
      editor_save_warnings(private$editor)
    }
  ),
  public = list(
    #' @description
    #' Creates a new livecode server
    #'
    #' @param file Path to file to broadcast.
    #' @param ip ip of the server, defaults to the top result of `network_interfaces()`.
    #' @param port port of the server, defaults to a random value.
    #' @param interval page update interval in seconds.
    #' @param auto_save should the broadcast file be auto saved each update tick.
    #' @param open_browser should a browser session be opened.
    initialize = function(
      file = NULL, ip = NULL, port = NULL, interval = 1,
      auto_save = TRUE, open_browser = TRUE
    ) {
      private$init_file(file, auto_save)
      private$init_ip(ip)
      private$init_port(port)
      private$init_interval(interval)

      self$start()

      if (auto_save)
        private$init_auto_save()

      if (open_browser)
        later::later(function() self$open(), 1)
    },

    #' @description
    #' Open server in browser
    open = function() {
      if (self$is_running())
        utils::browseURL(self$url, browser = get_browser())
      else
        cli::cli_abort("The server is not currently running!")
    },

    #' @description
    #' Class print method
    #' @param ... Ignored.
    print = function(...) {
      name = basename(private$file)
      url = self$url
      status = if (self$is_running()) "running" else "stopped"
      cli::cli_text("{.strong livecode server:} {.file {name}} @ {.url {url}} ({status})")
      invisible(self)
    },

    #' @description
    #' Send a noty message to all connected users on the next update tick.
    #' Messages queued while nobody is connected are held until someone
    #' connects.
    #'
    #' @param text text of the message.
    #' @param type message type (`alert`, `success`, `warning`, `error`, `info`).
    #' @param theme message theme, one of `mint`, `sunset`, `relax`, `nest`,
    #'   `metroui`, `semanticui`, `light`, `bootstrap-v3`, `bootstrap-v4`
    #'   (See [here](https://ned.im/noty/#/themes) for examples).
    #' @param layout message location.
    #' @param ... additional noty arguments, for example `timeout`, the time
    #'   in milliseconds before the message closes (10000 by default, `FALSE`
    #'   keeps it open until it is clicked).
    #' @param parse_md should message text be processed as markdown before sending.
    #' @param sticky should the message also be shown to every user who
    #'   connects later. Only one sticky message is kept, a new one replaces it.
    send_msg = function(text,
                        type = "info",
                        theme = "bootstrap-v4",
                        layout = "topRight",
                        ...,
                        parse_md = TRUE,
                        sticky = FALSE) {
      if (parse_md) {
        text = markdown::markdownToHTML(
          text = text,
          fragment.only = TRUE
        )
      } else {
        text = paste(text, collapse = "\n")
      }

      args = c(
        list(text = text, type = type, theme = theme, layout = layout),
        list(...)
      )

      text_has_link = grepl("<a ", text)
      closeWith_used = "closeWith" %in% names(args)

      if (text_has_link && !closeWith_used)
        args[["closeWith"]] = list("button")

      msg = do.call(noty_msg$new, args)

      if (sticky) {
        private$sticky_msg = msg
        if (!is.null(private$server))
          private$server$set_sticky_msg(msg)
      } else if (!is.null(private$server)) {
        private$server$add_msg(msg)
      }

      invisible(self)
    },

    #' @description
    #' Determine if the server is running.
    #' @return Returns `TRUE` if the server is running.
    is_running = function() {
      !is.null(private$server) && private$server$is_running()
    },

    #' @description
    #' Start the server
    start = function() {
      if (self$is_running()) {
        cli::cli_alert_info("The server is already running at {.url {self$url}}.")
        return(invisible(self))
      }

      private$server = tryCatch(
        file_stream_server(
          private$ip, private$port, private$file, private$file_id,
          interval = private$interval, auto_save = private$auto_save,
          editor = private$editor
        ),
        error = function(e) {
          cli::cli_abort(
            "Failed to start the server at {.url {self$url}}.",
            parent = e
          )
        }
      )

      if (!is.null(private$sticky_msg))
        private$server$set_sticky_msg(private$sticky_msg, announce = FALSE)

      name = basename(private$file)
      cli::cli_alert_success("Started sharing {.file {name}} at {.url {self$url}}.")

      if (is_ip_private(private$ip)) {
        cli::cli_alert_warning(paste(
          "The current ip address ({.val {private$ip}}) for the server is private,",
          "only users on the same local network are likely to be able to connect."
        ))
      }

      if (!is_registered(self))
        register_server(self)

      invisible(self)
    },

    #' @description
    #' Stop the server
    #'
    #' @param warn Should the users be sent a warning that the server is shutting down.
    stop = function(warn = FALSE) {
      if (!self$is_running()) {
        deregister_server(self)
        cli::cli_alert_info("The server at {.url {self$url}} is not running.")
        return(invisible(self))
      }

      if (warn && private$server$n_clients() > 0) {
        msg = noty_msg$new("Server is shutting down!", type = "error")
        private$server$broadcast(list(
          interval = private$interval,
          messages = list(msg$get_msg())
        ))
        # httpuv writes from its own thread, give it a moment before the
        # sockets are closed.
        Sys.sleep(0.5)
      }

      private$server$stop()
      deregister_server(self)

      cli::cli_alert_success("Stopped server at {.url {self$url}}.")
      invisible(self)
    },

    #' @description
    #' Restart the server
    restart = function() {
      if (self$is_running()) {
        self$stop()
      }

      self$start()

      invisible(self)
    }
  ),
  active = list(
    #' @field url The current url of the server.
    url = function() {
      paste0("http://", private$ip, ":", private$port)
    },

    #' @field path The path of the file being served.
    path = function() {
      private$file
    }
  )
)

#' Create a livecode server for broadcasting a file
#'
#' @description
#' Starts a local \code{livecode} server that streams a syntax-highlighted
#' version of a source file and automatically updates it in connected browsers.
#' The server uses a WebSocket connection to push updates at a fixed interval.
#'
#' @param file Path to the file to broadcast. If not provided and running in
#'   RStudio, the currently active document is used.
#' @param ip IPv4 address for the local server. Defaults to the first available
#'   network interface (see \code{network_interfaces()}).
#' @param port Port for the local server. Defaults to a random available port.
#' @param interval Numeric. Update interval in seconds for refreshing content
#'   and pushing updates to clients.
#' @param auto_save Logical. Should the source file be automatically saved at
#'   each update interval (RStudio only).
#' @param open_browser Logical. Should a browser session be opened automatically.
#'
#' @details
#' The server is implemented using \pkg{httpuv} and maintains an open WebSocket
#' connection to all clients. Updates are pushed at the specified \code{interval},
#' including:
#' \itemize{
#'   \item Updated file contents
#'   \item Cursor/selection position (RStudio only)
#'   \item Messages sent via \code{send_msg()}
#' }
#'
#' The page highlights the file with a bundled copy of Prism, chosen by the
#' file's extension: R, Quarto and R Markdown (\code{.qmd}, \code{.Rmd}),
#' Python, SQL, shell, Markdown, YAML, JSON, TOML, JavaScript, TypeScript,
#' CSS, HTML and XML, C, C++, Java, Julia, Rust, Go, LaTeX, Dockerfiles and
#' Makefiles. Other files are shown as plain text. Line numbers match the
#' source file, and the lines selected in the editor are highlighted and
#' scrolled into view. The page reconnects by itself if the connection to
#' the server drops.
#'
#' A welcome message is shown to every user when they connect.
#'
#' @return
#' An invisible \code{LiveCodeServer_Interface} R6 object representing the running
#' server. This object provides methods such as:
#' \itemize{
#'   \item \code{$stop()} to stop the server
#'   \item \code{$restart()} to restart the server
#'   \item \code{$send_msg()} to send messages to connected clients
#' }
#'
#' @examples
#' \dontrun{
#' srv = serve_file("script.R")
#'
#' srv$send_msg("Hello, everyone!", type = "success")
#'
#' srv$stop()
#' }
#'
#' @export
serve_file = function(file = NULL,
                      ip = NULL,
                      port = NULL,
                      interval = 1,
                      auto_save = TRUE,
                      open_browser = TRUE) {
  server = lc_server_iface$new(
    file = file, ip = ip, port = port,
    interval = interval,
    auto_save = auto_save, open_browser = open_browser
  )

  welcome_msg = c(
    "## Welcome to `livecode`!",
    "",
    paste0("Serving `", basename(server$path), "` at"),
    "",
    paste0(
      "<div class='server_link'><a href='", server$url, "'>", server$url, "</a></div>"
    )
  )

  server$send_msg(text = welcome_msg, sticky = TRUE)
  invisible(server)
}

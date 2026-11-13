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
      private$by_id = identical(editor, "rstudio") && !is.null(file_id)
      private$cache = file_cache(file)
      private$disk_seen = private$cache$content
      private$state = list(content = private$disk_seen, selection = NULL)
      private$websockets = new.env(parent = emptyenv())
      private$sinks = list()

      private$handle = httpuv::startServer(host, port, private$app())

      later::later(function() self$tick(), 0)
    },

    is_running = function() {
      !is.null(private$handle) && private$handle$isRunning()
    },

    # Pages are told that the session is over before their sockets close, so
    # they can say so instead of that they are reconnecting.
    stop = function() {
      if (self$is_running()) {
        close_websockets(private$websockets, list(status = "ended"))
        httpuv::stopServer(private$handle)
      }
      if (!is.null(private$local_handle) && private$local_handle$isRunning())
        httpuv::stopServer(private$local_handle)
      private$local_handle = NULL
      invisible(self)
    },

    # The same page on 127.0.0.1, for the instructor's own IDE: its Viewer
    # shows pages of this computer only (RStudio), or reaches the session
    # through a proxy that cannot get to the network address (Posit
    # Workbench). It lives under a token, as a follower's page does, because
    # such a proxy's address is shared by every user of the service.
    serve_locally = function(port, token) {
      if (!self$is_running())
        return(NULL)
      if (is.null(private$local_handle)) {
        private$local_handle = httpuv::startServer(
          "127.0.0.1", port, private$local_app(paste0("/", token, "/"))
        )
      }
      paste0("http://127.0.0.1:", port, "/", token, "/")
    },

    # The served file, which follows an RStudio document through a rename.
    path = function() {
      private$file
    },

    # Positron answers editor requests only while a console command runs, so
    # its tick reads the editor only while the blocking loop is running.
    set_blocking = function(blocking) {
      private$blocking = isTRUE(blocking)
      invisible(self)
    },

    n_clients = function() {
      length(ls(private$websockets))
    },

    # A sink is a function that receives every frame, as the websockets do.
    # It first receives the full current state.
    add_sink = function(name, sink) {
      private$sinks[[name]] = sink
      private$guard(name, function() sink(self$sink_frame()))
      invisible(self)
    },

    remove_sink = function(name) {
      private$sinks[[name]] = NULL
      private$errors[[name]] = NULL
      invisible(self)
    },

    has_sinks = function() {
      length(private$sinks) > 0
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
        self$emit(list(interval = private$interval, sticky = private$sticky_msg))
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
        frame[["messages"]] = list(private$sticky_msg)

      frame
    },

    # The full current state for a newly added sink.
    sink_frame = function() {
      list(
        interval = private$interval,
        content = private$state[["content"]],
        selection = if (is.null(private$state[["selection"]])) "" else private$state[["selection"]],
        sticky = private$sticky_msg
      )
    },

    # Builds the next update and records it as the broadcast state. The
    # content is compared with what was last broadcast rather than with the
    # previous read, so a client connecting in between cannot hide a change
    # from the others.
    build_frame = function() {
      frame = list(interval = private$interval)

      content = private$current_content()
      if (!identical(content, private$state[["content"]])) {
        frame[["content"]] = content
        private$state[["content"]] = content
      }

      selection = private$selection
      if (!is.null(selection) && !identical(selection, private$state[["selection"]])) {
        frame[["selection"]] = selection
        private$state[["selection"]] = selection
      }

      # Queued messages wait until someone is connected to see them. A
      # sink, such as the tailcat broadcast, counts as someone.
      if ((self$n_clients() > 0 || self$has_sinks()) && self$have_msgs()) {
        frame[["messages"]] = self$get_msgs()
      }

      frame
    },

    broadcast = function(frame) {
      if (self$n_clients() == 0)
        return(invisible(self))

      json = frame_to_json(ws_frame(frame))
      for (ws_id in ls(private$websockets)) {
        ws = private$websockets[[ws_id]]
        if (!is.null(ws))
          try(ws$send(json), silent = TRUE)
      }

      invisible(self)
    },

    # Sends a frame to the websockets and to every sink.
    emit = function(frame) {
      self$broadcast(frame)
      for (name in names(private$sinks)) {
        sink = private$sinks[[name]]
        private$guard(name, function() sink(frame))
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

      private$buffer_fresh = FALSE
      private$guard("Reading the editor", function() private$read_editor())

      if (private$auto_save && identical(private$editor, "rstudio")) {
        private$guard("Auto-save", function() {
          editor_save(private$file_id, private$editor)
        })
      }
      private$guard("Broadcast update", function() {
        frame = self$build_frame()
        self$emit(frame)
      })

      invisible(TRUE)
    }
  ),

  private = list(
    handle = NULL,
    local_handle = NULL,
    page = NULL,
    file = NULL,
    file_id = NULL,
    lang = NULL,
    interval = NULL,
    auto_save = FALSE,
    editor = "none",
    by_id = FALSE,
    blocking = FALSE,
    cache = NULL,
    disk_seen = NULL,
    buffer = NULL,
    buffer_fresh = FALSE,
    state = NULL,
    websockets = NULL,
    sinks = NULL,
    next_ws_id = 0L,
    msg_queue = list(),
    sticky_msg = NULL,
    errors = list(),
    selection = NULL,

    reads_editor = function() {
      identical(private$editor, "rstudio") ||
        (identical(private$editor, "positron") && private$blocking)
    },

    read_editor = function() {
      if (!private$reads_editor())
        return(invisible(FALSE))

      snap = editor_read(private$file, private$file_id, private$editor, by_id = private$by_id)
      if (is.null(snap))
        return(invisible(FALSE))

      if (!is.null(snap[["by_id_error"]])) {
        private$by_id = FALSE
        cli::cli_bullets(c("!" = paste(
          "livecode: cannot read {.file {basename(private$file)}} from RStudio by its document id",
          "({snap[['by_id_error']]}), so it is read only while it is the active document."
        )))
      }

      if (!is.null(snap[["contents"]])) {
        private$buffer = snap[["contents"]]
        private$buffer_fresh = TRUE
      }
      private$selection = snap[["selection"]]

      if (identical(private$editor, "rstudio") && is_saved_path(snap[["path"]]))
        private$follow(snap[["path"]])

      invisible(TRUE)
    },

    # The editor's buffer when there is one, else the file on disk. A buffer
    # not read this tick (another tab is active in Positron) is kept, unless
    # the file on disk changed since, which then is the newer text.
    current_content = function() {
      disk = private$cache$content
      if (!identical(disk, private$disk_seen)) {
        private$disk_seen = disk
        if (!private$buffer_fresh)
          private$buffer = NULL
      }
      private$buffer_fresh = FALSE

      if (is.null(private$buffer)) disk else private$buffer
    },

    # RStudio keeps a document's id through Rename and Save As, so the
    # document read by id is followed to its new path.
    follow = function(path) {
      if (same_path(path, private$file))
        return(invisible(FALSE))

      old = private$file
      private$file = path
      private$page = livecode_page(path, private$lang)
      if (file.exists(path)) {
        private$cache = file_cache(path)
        private$disk_seen = private$cache$content
      }

      cli::cli_bullets(c("i" = 
        "livecode: {.file {basename(old)}} was renamed or saved as {.file {path}}, livecode now serves {.file {basename(path)}}."
      ))
      invisible(TRUE)
    },

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
            cli::cli_bullets(c("x" = 
              "livecode: {label} failed, will keep retrying: {msg}"
            ))
          }
        }
      )
    },

    # The app again, with everything under `root` and nothing served
    # outside it. Its websockets join the others, so they get every frame.
    local_app = function(root) {
      app = private$app()
      refuse = function(status, text) {
        list(
          status = status,
          headers = list("Content-Type" = "text/plain; charset=UTF-8"),
          body = text
        )
      }
      inside = function(req) {
        path = req[["PATH_INFO"]]
        is_string(path) && startsWith(path, root)
      }
      static = list(pkg_resource("resources"))
      names(static) = paste0(root, "web")

      list(
        call = function(req) {
          if (!inside(req))
            return(refuse(403L, "Forbidden"))
          if (!identical(req[["PATH_INFO"]], root))
            return(refuse(404L, "Not found"))
          app$call(req)
        },
        onWSOpen = function(ws) {
          if (!inside(ws$request)) {
            ws$close()
            return(invisible())
          }
          app$onWSOpen(ws)
        },
        staticPaths = static
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

          ws$send(frame_to_json(ws_frame(self$connect_frame())))
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

# Frames carry noty_msg objects; the page gets their noty options. A sticky
# message is shown like any other.
ws_frame = function(frame) {
  msgs = c(frame[["messages"]], if (!is.null(frame[["sticky"]])) list(frame[["sticky"]]))
  frame[["sticky"]] = NULL
  frame[["messages"]] = if (length(msgs) > 0) lapply(msgs, function(m) m$get_msg())
  frame
}

frame_to_json = function(frame) {
  jsonlite::toJSON(frame, auto_unbox = TRUE)
}

# Sends a last frame to every websocket and closes it. httpuv writes from
# its own thread, so the sockets get a moment to drain before the caller
# stops the server.
close_websockets = function(websockets, frame, wait = 0.2) {
  ids = ls(websockets)
  if (length(ids) == 0)
    return(invisible())

  json = frame_to_json(frame)
  for (ws_id in ids) {
    ws = websockets[[ws_id]]
    if (!is.null(ws)) {
      try(ws$send(json), silent = TRUE)
      try(ws$close(), silent = TRUE)
    }
  }

  deadline = Sys.time() + wait
  while (length(ls(websockets)) > 0 && Sys.time() < deadline)
    httpuv::service(20)
  invisible()
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
#' @param file_id Optional RStudio document id of the file, used to read its
#'   buffer while another tab is active, to save it, and to follow it through
#'   a rename. Positron has no document ids.
#' @param interval Numeric update interval in seconds between broadcast ticks.
#' @param auto_save Logical. Should the document be saved in RStudio on each
#'   tick. Ignored in Positron and outside an IDE.
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
#'   \item Broadcasts the editor's buffer of the served document, unsaved
#'     edits included, when it can be read, and otherwise the file on disk,
#'     tracked with \code{\link{file_cache}}. Content is sent only when it
#'     differs from what was last broadcast.
#'   \item Pushes queued messages from the server message queue.
#'   \item In RStudio, reads the served document by id on every tick, so its
#'     buffer is broadcast while another tab is active and it is followed
#'     through a rename or Save As. If the editor cannot read a document by
#'     id, this is reported once and the buffer is read only while the
#'     served document is active. With \code{auto_save = TRUE} the document
#'     is also saved on every tick.
#'   \item In Positron, which answers editor requests only while a console
#'     command runs, the tick reads the active editor only while the
#'     blocking loop of \code{\link{serve_file}} runs (see
#'     \code{$set_blocking()}); otherwise it makes no editor calls and the
#'     file is read from disk. While another document is active the last
#'     buffer seen keeps being broadcast.
#'   \item Broadcasts the editor selection when the served document is the
#'     active one, and an empty selection when it is not.
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
#' \code{\link{serve_file}} is a wrapper around \code{$new()}, which also
#' sends the welcome message and, with \code{block = TRUE}, keeps running
#' until the broadcast is interrupted or stopped.
#'
#' @export
lc_server_iface = R6::R6Class(
  "LiveCodeServer_Interface",
  cloneable = FALSE,
  private = list(
    file = NULL,
    file_id = NULL,
    auto_save = FALSE,
    block = FALSE,
    editor = "none",
    ip = NULL,
    local_only = FALSE,
    port = NULL,
    interval = NULL,
    server = NULL,
    sticky_msg = NULL,
    tailcat = NULL,
    local_page = NULL,

    # The address to show in the IDE's Viewer. A server on a network address
    # is also served on 127.0.0.1 for it, at a port and token that are kept
    # through restarts so an open page finds the server again.
    viewer_url = function() {
      if (identical(private$editor, "none") || isFALSE(getOption("livecode.viewer", TRUE)) ||
          is_ip_loopback(private$ip))
        return(NULL)

      if (is.null(private$local_page)) {
        private$local_page = list(
          port = httpuv::randomPort(host = "127.0.0.1"),
          token = random_token()
        )
      }
      tryCatch(
        private$server$serve_locally(private$local_page$port, private$local_page$token),
        error = function(e) NULL
      )
    },

    attach_tailcat = function() {
      broadcast = private$tailcat
      private$server$add_sink("Tailcat broadcast", function(frame) broadcast$send(frame))
    },

    stop_server = function() {
      if (!is.null(private$server))
        private$server$stop()
    },

    init_file = function(file, auto_save, block) {
      editor = editor_backend()
      auto_save = check_auto_save(auto_save, editor, call = NULL)
      block = resolve_block(block, editor, call = NULL)
      ctx = NULL

      if (!is.null(file)) {
        if (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file))
          cli::cli_abort("{.arg file} must be a single file path.")

        file = path.expand(file)
        if (!file.exists(file))
          cli::cli_abort("Unable to locate file {.file {file}}.")
        file = normalizePath(file, winslash = "/")

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
      private$auto_save = auto_save
      private$block = block
      private$editor = editor
    },

    init_ip = function(ip, local_only = FALSE, tailcat = FALSE) {
      if (!isTRUE(local_only) && !isFALSE(local_only))
        cli::cli_abort("{.arg local_only} must be {.code TRUE} or {.code FALSE}.")

      if (local_only) {
        if (isFALSE(tailcat)) {
          cli::cli_abort(c(
            "{.code local_only = TRUE} needs a tailcat broadcast.",
            "i" = "A page on this computer only would reach nobody else: also pass {.code tailcat = TRUE}, or keys from {.fn livecode::tailcat_keys}."
          ))
        }
        if (!is.null(ip)) {
          cli::cli_abort(
            "Pass either {.arg ip} or {.code local_only = TRUE}, not both: a local only server always uses {.val 127.0.0.1}."
          )
        }
        ip = "127.0.0.1"
      }
      private$local_only = local_only

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

    init_editor_notes = function() {
      editor_notes(private$editor, private$auto_save, private$block)
    },

    send_welcome = function() {
      name = basename(self$path)
      # A local only page has no address worth showing: only the instructor
      # can open it.
      welcome_msg = if (private$local_only) {
        c("## Welcome to `livecode`!", "", paste0("Serving `", name, "`"))
      } else {
        c(
          "## Welcome to `livecode`!",
          "",
          paste0("Serving `", name, "` at"),
          "",
          paste0(
            "<div class='server_link'><a href='", self$url, "'>", self$url, "</a></div>"
          )
        )
      }

      self$send_msg(
        text = welcome_msg, sticky = TRUE,
        plain_text = paste0("Welcome to livecode! Following ", name, ".")
      )
    },

    start_server = function() {
      if (self$is_running()) {
        cli::cli_bullets(c("i" = "The server is already running at {.url {self$url}}."))
        return(invisible(FALSE))
      }

      if (!is.null(private$server))
        private$file = private$server$path()

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

      if (!is.null(private$tailcat))
        private$attach_tailcat()

      # A page already open in the Viewer reconnects to the same address.
      if (!is.null(private$local_page))
        private$viewer_url()

      name = basename(private$file)
      cli::cli_bullets(c("v" = "Started sharing {.file {name}} at {.url {self$url}}."))

      if (private$local_only) {
        cli::cli_bullets(c("i" = paste(
          "The page is served on this computer only,",
          "everyone else follows the tailcat broadcast."
        )))
      } else if (is_ip_private(private$ip)) {
        cli::cli_bullets(c("!" = paste(
          "The current ip address ({.val {private$ip}}) for the server is private,",
          "only users on the same local network are likely to be able to connect."
        )))
      }

      if (!is_registered(self))
        register_server(self)

      invisible(TRUE)
    },

    # Runs the event loop in this console until the server stops or the
    # call is interrupted. In Positron this keeps a console request active,
    # which is what lets the tick read the editor.
    block_loop = function() {
      server = private$server
      if (is.null(server) || !server$is_running())
        return(invisible(self))

      if (identical(private$editor, "positron"))
        positron_blocking_note()

      server$set_blocking(TRUE)
      on.exit(server$set_blocking(FALSE), add = TRUE)

      outcome = tryCatch(
        {
          while (server$is_running())
            block_step()
          NULL
        },
        interrupt = function(e) "interrupted",
        error = function(e) e
      )

      if (!is.null(outcome)) {
        suppressMessages(self$stop())
        name = basename(self$path)
        if (inherits(outcome, "error")) {
          cli::cli_bullets(c("x" = 
            "Stopped sharing {.file {name}} after an error: {conditionMessage(outcome)}"
          ))
        } else {
          cli::cli_bullets(c("v" = "Stopped sharing {.file {name}} at {.url {self$url}}."))
        }
      }

      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Creates a new livecode server
    #'
    #' @param file Path to file to broadcast, by default the active document
    #'   in RStudio or Positron.
    #' @param ip ip of the server, defaults to the top result of `network_interfaces()`.
    #' @param local_only should the page be served on this computer only
    #'   (`127.0.0.1`), so there is no network address to share and everyone
    #'   else follows the tailcat broadcast. Needs `tailcat` and cannot be
    #'   combined with `ip`.
    #' @param port port of the server, defaults to a random value.
    #' @param interval page update interval in seconds.
    #' @param auto_save should the served document be saved in RStudio at
    #'   each update tick. Not supported in Positron, see
    #'   \code{\link{serve_file}}.
    #' @param open_browser should the page be opened, in the IDE's Viewer
    #'   pane when it can show the page and else in the system browser, see
    #'   \code{\link{serve_file}}.
    #' @param tailcat also broadcast over tailcat: `FALSE` (the default),
    #'   `TRUE` for a fresh address, or keys from [tailcat_keys()] for an
    #'   address posted ahead of time (see `$start_tailcat()`).
    #' @param block should the call keep running, driving the broadcast, until
    #'   it is interrupted or the server is stopped. `NULL` (the default)
    #'   means `TRUE` in Positron and `FALSE` elsewhere, see
    #'   \code{\link{serve_file}}. With `block = TRUE`, `$start()` and
    #'   `$restart()` block too.
    initialize = function(
      file = NULL, ip = NULL, port = NULL, interval = 1,
      auto_save = FALSE, open_browser = TRUE, tailcat = FALSE, block = NULL,
      local_only = FALSE
    ) {
      if (!isFALSE(tailcat)) {
        tc_check_installed(tc_broadcast_packages(), "A tailcat broadcast", call = NULL)
        if (!isTRUE(tailcat))
          tc_key_list(tailcat, call = NULL)
      }

      private$init_file(file, auto_save, block)
      private$init_ip(ip, local_only, tailcat)
      private$init_port(port)
      private$init_interval(interval)

      private$start_server()

      if (!isFALSE(tailcat)) {
        tryCatch(
          self$start_tailcat(key = if (isTRUE(tailcat)) NULL else tailcat),
          error = function(e) {
            self$stop()
            stop(e)
          }
        )
      }

      private$init_editor_notes()
      private$send_welcome()

      # Positron answers the viewer request only while this call runs.
      if (open_browser && identical(private$editor, "positron"))
        self$open()
      else if (open_browser)
        later::later(function() if (self$is_running()) self$open(), 1)

      if (private$block)
        private$block_loop()
    },

    #' @description
    #' Open the page in the IDE's Viewer pane, or in the browser. In RStudio
    #' and Positron a server on a network address is shown in the Viewer
    #' through a copy of the page on `127.0.0.1`, see
    #' \code{\link{serve_file}}.
    open = function() {
      if (!self$is_running())
        cli::cli_abort("The server is not currently running!")

      local = private$viewer_url()
      if (is.null(local))
        editor_show_page(self$url, private$editor)
      else
        editor_show_page(local, private$editor, local = TRUE)
    },

    #' @description
    #' Class print method
    #' @param ... Ignored.
    print = function(...) {
      name = basename(self$path)
      url = self$url
      status = if (self$is_running()) "running" else "stopped"
      cli::cli_text("{.strong livecode server:} {.file {name}} @ {.url {url}} ({status})")
      if (!is.null(private$tailcat)) {
        n = private$tailcat$n_clients()
        state = if (private$tailcat$is_running()) "running, {n} follower{?s}" else "stopped"
        cli::cli_text(paste0("{.strong tailcat broadcast:} ", state))
      }
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
    #' @param plain_text the text shown to tailcat followers, whose page shows
    #'   messages as plain text rather than HTML. Defaults to `text` as
    #'   written, before any markdown processing.
    send_msg = function(text,
                        type = "info",
                        theme = "bootstrap-v4",
                        layout = "topRight",
                        ...,
                        parse_md = TRUE,
                        sticky = FALSE,
                        plain_text = NULL) {
      if (is.null(plain_text))
        plain_text = text
      plain_text = paste(plain_text, collapse = "\n")

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
        list(...),
        list(plain_text = plain_text)
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
    #' Start the server. With `block = TRUE` this call then keeps running
    #' until it is interrupted or the server is stopped.
    start = function() {
      private$start_server()
      if (private$block)
        private$block_loop()
      invisible(self)
    },

    #' @description
    #' Stop the server
    #'
    #' @param warn Should the users be sent a warning that the server is shutting down.
    stop = function(warn = FALSE) {
      if (!self$is_running()) {
        if (!is.null(private$tailcat))
          self$stop_tailcat()
        deregister_server(self)
        cli::cli_bullets(c("i" = "The server at {.url {self$url}} is not running."))
        return(invisible(self))
      }

      if (warn && (private$server$n_clients() > 0 || private$server$has_sinks())) {
        msg = noty_msg$new("Server is shutting down!", type = "error")
        private$server$emit(list(
          interval = private$interval,
          messages = list(msg)
        ))
        # httpuv writes from its own thread, give it a moment before the
        # sockets are closed.
        Sys.sleep(0.5)
      }

      private$stop_server()
      if (!is.null(private$tailcat))
        self$stop_tailcat()
      deregister_server(self)

      cli::cli_bullets(c("v" = "Stopped server at {.url {self$url}}."))
      invisible(self)
    },

    #' @description
    #' Restart the server. A tailcat broadcast keeps running, with the same
    #' address. With `block = TRUE` this call then keeps running, as
    #' `$start()` does.
    restart = function() {
      if (self$is_running()) {
        private$stop_server()
        deregister_server(self)
        cli::cli_bullets(c("v" = "Stopped server at {.url {self$url}}."))
      }

      self$start()

      invisible(self)
    },

    #' @description
    #' Also broadcast the file over tailcat, so students anywhere can follow
    #' it with [follow()]. The broadcast runs in a separate R process and
    #' needs the rtailcat package; see the section on tailcat in
    #' [serve_file()].
    #'
    #' @param key `NULL` for a fresh address, or keys from [tailcat_keys()]
    #'   (or [rtailcat::tailcat_keygen()]) for an address posted ahead of
    #'   time. Use a set of keys for one class meeting only.
    #' @param port tailcat port to listen on.
    #' @param max_clients most followers connected at once, further
    #'   connections are closed straight away.
    start_tailcat = function(key = NULL, port = 4000, max_clients = 150) {
      tc_check_installed(tc_broadcast_packages(), "A tailcat broadcast")

      if (!self$is_running())
        cli::cli_abort("The server is not running, start it with {.code $start()} first.")

      if (!is.null(private$tailcat) && private$tailcat$is_running()) {
        cli::cli_bullets(c("i" = "The tailcat broadcast is already running, see {.field $tailcat_address}."))
        return(invisible(self))
      }

      port = suppressWarnings(as.integer(port))
      if (length(port) != 1 || is.na(port) || port < 1L || port > 65535L)
        cli::cli_abort("{.arg port} must be a single port number between 1 and 65535.")

      max_clients = suppressWarnings(as.integer(max_clients))
      if (length(max_clients) != 1 || is.na(max_clients) || max_clients < 1L)
        cli::cli_abort("{.arg max_clients} must be a single positive whole number.")

      keys = if (is.null(key)) NULL else tc_key_list(key, call = environment())
      expected = if (inherits(key, "livecode_tailcat_keys")) key[["address"]] else NULL

      if (!is.null(private$tailcat))
        self$stop_tailcat()

      private$tailcat = tc_broadcast$new(
        self$path, file_code_language(self$path), private$interval,
        key = keys, port = port, max_clients = max_clients
      )
      private$attach_tailcat()

      address = private$tailcat$address()
      if (!is.null(expected) && !identical(address, expected)) {
        cli::cli_bullets(c("!" = paste(
          "The broadcast's address differs from the one saved with these keys,",
          "probably because tailcat's relay map changed. Post the new address below."
        )))
      }

      cli::cli_bullets(c("v" = "Tailcat broadcast ready. Students run:"))
      cli::cli_code(paste0("livecode::follow(\"", address, "\")"))
      cli::cli_bullets(c("i" = paste(
        "The address is a credential: post it where only you can edit,",
        "such as the course site, not in a shared document or chat."
      )))

      invisible(self)
    },

    #' @description
    #' Stop the tailcat broadcast. The server itself keeps running.
    stop_tailcat = function() {
      if (is.null(private$tailcat)) {
        cli::cli_bullets(c("i" = "No tailcat broadcast is running."))
        return(invisible(self))
      }

      if (!is.null(private$server))
        private$server$remove_sink("Tailcat broadcast")
      private$tailcat$stop()
      private$tailcat = NULL

      cli::cli_bullets(c("v" = "Stopped the tailcat broadcast."))
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
      if (is.null(private$server)) private$file else private$server$path()
    },

    #' @field tailcat_address The tailcat address of the broadcast, or
    #'   `NULL` when there is none. It is a credential, see
    #'   `$start_tailcat()`.
    tailcat_address = function() {
      if (is.null(private$tailcat))
        return(NULL)
      private$tailcat$address()
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
#'   RStudio or Positron, the active document is used; it must have been saved
#'   to a file. When a path is given in an IDE, the file is opened in the
#'   editor first.
#' @param ip IPv4 address for the local server. Defaults to the first available
#'   network interface (see \code{network_interfaces()}).
#' @param port Port for the local server. Defaults to a random available port.
#' @param local_only Logical. Serve the page on this computer only
#'   (\code{127.0.0.1}), so there is no network address to share and
#'   everyone else follows the tailcat broadcast. Only allowed together with
#'   \code{tailcat}, and not with \code{ip}.
#' @param interval Numeric. Update interval in seconds for refreshing content
#'   and pushing updates to clients.
#' @param auto_save Logical. Should the served document also be saved at each
#'   update interval. RStudio only and off by default, since the editor's
#'   unsaved text is broadcast anyway; \code{TRUE} is an error in Positron.
#'   See the section on editors.
#' @param open_browser Logical. Should the page be opened automatically: in
#'   the Viewer pane in RStudio and Positron, and otherwise in the system
#'   browser. Set \code{options(livecode.viewer = FALSE)} to always use the
#'   browser. See the section on editors.
#' @param tailcat Also broadcast over tailcat, so students anywhere can follow
#'   with [follow()]: `FALSE` (the default), `TRUE` for a fresh address, or
#'   keys from [tailcat_keys()] for an address posted ahead of time. Needs
#'   the rtailcat package, see the section on tailcat below.
#' @param block Logical or `NULL`. Should \code{serve_file()} keep running,
#'   driving the broadcast from this console, until it is interrupted (Esc
#'   or Ctrl-C) or the server is stopped. `NULL` (the default) means `TRUE`
#'   in Positron, which needs it to read the editor, and `FALSE` elsewhere.
#'   See the section on editors.
#'
#' @details
#' The server is implemented using \pkg{httpuv} and maintains an open WebSocket
#' connection to all clients. Updates are pushed at the specified \code{interval},
#' including:
#' \itemize{
#'   \item The text of the file, taken from the editor (unsaved edits
#'     included) when it can be read and from the file on disk otherwise
#'   \item The lines selected in the editor
#'   \item Messages sent via \code{send_msg()}
#' }
#'
#' @section Editors:
#' In RStudio and Positron the page shows the editor's text of the served
#' document, including edits that have not been saved, and highlights the
#' lines selected in the editor while the served document is the active one;
#' the highlight is cleared when another document is active. Outside an IDE
#' the page shows the file on disk and updates whenever it is saved. The two
#' IDEs differ in a few ways:
#' \itemize{
#'   \item RStudio is read at every update from the idle event loop, so
#'     \code{serve_file()} returns straight away (\code{block = FALSE}). The
#'     served document is read by its id, so its text keeps being broadcast
#'     while another tab is active and it is followed through a rename or
#'     Save As. If RStudio cannot read a document by id, this is reported
#'     once and the text is read only while the served document is active.
#'   \item Positron answers editor requests only while a console command is
#'     running. So by default (\code{block = TRUE}) \code{serve_file()}
#'     keeps running and drives the broadcast itself: the console it was
#'     called from is dedicated to the broadcast until you interrupt it with
#'     Esc or Ctrl-C, which stops the server, and you work in a second R
#'     console session meanwhile. Positron exposes only the active editor,
#'     so while another tab is active the page keeps the last text seen
#'     and clears the highlight. Renames are not followed in Positron.
#'   \item With \code{block = FALSE} in Positron, livecode makes no editor
#'     calls after startup: the page shows the file on disk, with no
#'     highlight. Turning on Positron's auto save (\code{files.autoSave})
#'     keeps the file on disk close to what you type.
#'   \item Saving is not needed for the broadcast. In RStudio,
#'     \code{auto_save = TRUE} also saves the served document at every
#'     update, and a warning is printed when "Strip trailing horizontal
#'     whitespace when saving" is on, globally or in the project. In
#'     Positron \code{auto_save = TRUE} is an error; use Positron's own
#'     auto save setting instead.
#' }
#' In an IDE your own copy of the page opens in the Viewer pane. The Viewer
#' cannot always reach the server's network address: RStudio's shows pages
#' of this computer only, and in a Posit Workbench session the browser
#' reaches the session through Workbench, not through that address. So the
#' server also serves the page on \code{127.0.0.1} for the Viewer, under a
#' random token in its address without which nothing is served there.
#' Students keep using the network address or the tailcat broadcast.
#'
#' While \code{serve_file()} blocks, its console is busy, so
#' \code{$send_msg()} and the other methods of the server cannot be called
#' from that console. Use \code{block = FALSE} if you need them (in Positron
#' at the cost of reading the file from disk).
#'
#' The editor is detected automatically. Set
#' \code{options(livecode.editor = "rstudio")}, \code{"positron"}, or
#' \code{"none"} to override the detection, for example \code{"none"} to
#' serve a file without involving the editor at all. Blocking also works
#' outside an IDE, for example from \command{Rscript}, and in RStudio when
#' asked for with \code{block = TRUE}.
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
#' When the server stops, the page says that the session has ended instead
#' of that it is reconnecting. It still retries quietly, so it picks up
#' again if the server is restarted at the same address.
#'
#' @section Broadcasting over tailcat:
#' The page is served on the local network only. With \code{tailcat = TRUE}
#' (or \code{$start_tailcat()} on a running server) the file is also
#' broadcast over \href{https://github.com/tailscale/tailcat}{tailcat}, an
#' end to end encrypted transport that reaches students anywhere, directly
#' or through tailcat's public relays. \code{serve_file()} prints the
#' address, and each student runs \code{livecode::follow("tc...")} with it
#' to see the same page, served by their own R session.
#'
#' The broadcast runs in a separate R process, so a problem in it cannot
#' take down the instructor's session, and it keeps sending while the
#' session is busy. It only ever sends; nothing a follower sends is read.
#' Followers that fall behind are dropped and reconnect. At most
#' \code{max_clients} (150 by default, see \code{$start_tailcat()})
#' followers are connected at once.
#'
#' With \code{local_only = TRUE} the page itself is served on
#' \code{127.0.0.1} only, for the instructor's own use, and the tailcat
#' broadcast is the only way in for everyone else. Use it on networks where
#' the local address would not be reachable anyway, or should not be shared.
#'
#' When the broadcast is stopped (\code{$stop()}, \code{$stop_tailcat()},
#' or interrupting a blocking \code{serve_file()}), followers are told that
#' the session has ended and stop reconnecting; to follow a broadcast that
#' is started again, they run \code{follow()} again.
#'
#' Practical points:
#' \itemize{
#'   \item The address is a credential: anyone who has it can follow the
#'     session. Post it where only you can edit, such as the course site or
#'     an LMS announcement, not a shared document or a chat, where someone
#'     could replace it with the address of their own broadcast.
#'   \item By default each broadcast has a fresh address that stops working
#'     when it stops. To post an address before class, create keys with
#'     \code{\link[=tailcat_keys]{tailcat_keys()}} and pass them as
#'     \code{tailcat}. Use fresh keys for each class meeting, since everyone
#'     who ever had an address can use it for as long as its keys are used.
#'   \item Students need the rtailcat package, which is not on CRAN and
#'     needs a Go toolchain to build (\code{pak::pak("rundel/rtailcat")}).
#'   \item Followers see messages as plain text (see the \code{plain_text}
#'     argument of \code{$send_msg()}).
#'   \item Do a dry run in the room before relying on it. If the network
#'     keeps devices apart, traffic goes through the public relays, which
#'     are rate limited and have no uptime guarantee.
#' }
#'
#' @return
#' An invisible \code{LiveCodeServer_Interface} R6 object representing the running
#' server. This object provides methods such as:
#' \itemize{
#'   \item \code{$stop()} to stop the server
#'   \item \code{$restart()} to restart the server
#'   \item \code{$send_msg()} to send messages to connected clients
#'   \item \code{$start_tailcat()}, \code{$stop_tailcat()}, and
#'     \code{$tailcat_address} for the tailcat broadcast
#' }
#' With \code{block = TRUE} the object is returned once the broadcast has
#' stopped.
#'
#' @examples
#' \dontrun{
#' # RStudio, or any R session with block = FALSE: returns straight away
#' srv = serve_file("script.R", block = FALSE)
#'
#' srv$send_msg("Hello, everyone!", type = "success")
#'
#' srv$stop()
#'
#' # Positron: runs until interrupted with Esc or Ctrl-C
#' serve_file("script.R")
#' }
#'
#' @export
serve_file = function(file = NULL,
                      ip = NULL,
                      port = NULL,
                      interval = 1,
                      auto_save = FALSE,
                      open_browser = TRUE,
                      tailcat = FALSE,
                      block = NULL,
                      local_only = FALSE) {
  server = lc_server_iface$new(
    file = file, ip = ip, port = port,
    interval = interval,
    auto_save = auto_save, open_browser = open_browser,
    tailcat = tailcat, block = block, local_only = local_only
  )
  invisible(server)
}

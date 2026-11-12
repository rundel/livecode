# The follower's local page: an httpuv server on 127.0.0.1 that serves the
# same page as the instructor's server and relays validated frames to it
# over a websocket. It never takes input from the page.
follower_view = R6::R6Class(
  "livecode_follower_view",
  cloneable = FALSE,
  public = list(
    initialize = function(port = NULL) {
      host = "127.0.0.1"
      if (is.null(port))
        port = httpuv::randomPort(host = host)
      private$port = as.integer(port)
      private$state = list(file = "", lang = "none", content = "", selection = "", status = "waiting")
      private$websockets = new.env(parent = emptyenv())
      private$handle = httpuv::startServer(host, private$port, private$app())
    },

    url = function() {
      paste0("http://127.0.0.1:", private$port)
    },

    file = function() {
      private$state$file
    },

    is_running = function() {
      !is.null(private$handle) && private$handle$isRunning()
    },

    n_clients = function() {
      length(ls(private$websockets))
    },

    set_hello = function(hello) {
      private$state$file = hello$file
      private$state$lang = hello$lang
      private$send(list(lang = hello$lang))
    },

    update = function(frame) {
      for (field in intersect(names(frame), c("content", "selection")))
        private$state[[field]] = frame[[field]]
      private$send(frame)
    },

    set_status = function(status) {
      if (identical(status, private$state$status))
        return(invisible(self))
      private$state$status = status
      private$send(list(status = status))
    },

    stop = function() {
      if (self$is_running())
        httpuv::stopServer(private$handle)
      invisible(self)
    }
  ),

  private = list(
    handle = NULL,
    port = NULL,
    state = NULL,
    websockets = NULL,
    next_ws_id = 0L,

    send = function(frame) {
      json = frame_to_json(frame)
      for (ws_id in ls(private$websockets)) {
        ws = private$websockets[[ws_id]]
        if (!is.null(ws))
          try(ws$send(json), silent = TRUE)
      }
      invisible()
    },

    connect_frame = function() {
      private$state[c("status", "lang", "content", "selection")]
    },

    # Only this machine's own page may use the server. Checking the Host
    # header stops DNS rebinding, and checking the Origin of a websocket
    # stops any other web page open in the browser from reading the session.
    allowed_hosts = function() {
      paste0(c("127.0.0.1", "localhost"), ":", private$port)
    },

    host_ok = function(req) {
      host = req[["HTTP_HOST"]]
      !is.null(host) && host %in% private$allowed_hosts()
    },

    origin_ok = function(req) {
      origin = req[["HTTP_ORIGIN"]]
      is.null(origin) || origin %in% paste0("http://", private$allowed_hosts())
    },

    app = function() {
      list(
        call = function(req) {
          if (!private$host_ok(req)) {
            return(list(
              status = 403L,
              headers = list("Content-Type" = "text/plain; charset=UTF-8"),
              body = "Forbidden"
            ))
          }
          file = if (nzchar(private$state$file)) private$state$file else "livecode"
          list(
            status = 200L,
            headers = list("Content-Type" = "text/html; charset=UTF-8"),
            body = livecode_page(file, private$state$lang, message_mode = "text")
          )
        },

        onWSOpen = function(ws) {
          if (!private$host_ok(ws$request) || !private$origin_ok(ws$request)) {
            ws$close()
            return(invisible())
          }

          private$next_ws_id = private$next_ws_id + 1L
          ws_id = paste0("ws", private$next_ws_id)
          private$websockets[[ws_id]] = ws

          ws$onClose(function() {
            if (exists(ws_id, envir = private$websockets, inherits = FALSE))
              rm(list = ws_id, envir = private$websockets)
          })

          ws$send(frame_to_json(private$connect_frame()))
        },

        staticPaths = list(
          "/web" = pkg_resource("resources")
        )
      )
    }
  )
)

# Follows a tailcat broadcast. Everything received is text to display:
# lines are capped in length, every field is validated and rebuilt, and
# nothing is ever evaluated, sourced, or written to disk. The follower only
# reads from the connection; it never writes to it.
#
# Each attempt uses a new client from new_client(): a client that knew a
# broadcast before it restarted cannot reach the restarted one, even with
# the same keys.
lc_follower = R6::R6Class(
  "LiveCodeFollower",
  cloneable = FALSE,
  public = list(
    initialize = function(new_client, port = tc_default_port, local_port = NULL,
                          dial_timeout = 30) {
      private$new_client = new_client
      private$port = port
      private$dial_timeout = dial_timeout
      private$view = follower_view$new(local_port)
      private$retry_delay = tc_retry_min
      self$reset_stream()
      private$connect()
    },

    # Bytes from the broadcast, as the read loop passes them on.
    receive = function(bytes) {
      lines = private$splitter(bytes)
      for (line in lines)
        private$handle_line(line)
      invisible(self)
    },

    reset_stream = function() {
      private$splitter = tc_line_splitter()
      private$have_hello = FALSE
      invisible(self)
    },

    is_running = function() {
      !private$stopped && private$view$is_running()
    },

    is_connected = function() {
      !is.null(private$conn) && private$have_hello
    },

    stop = function() {
      if (private$stopped)
        return(invisible(self))
      private$stopped = TRUE
      private$gen = private$gen + 1L
      if (!is.null(private$cancel_retry))
        private$cancel_retry()
      private$close_conn()
      private$close_client()
      private$view$stop()
      deregister_follower(self)
      cli::cli_bullets(c("v" = "Stopped following the broadcast."))
      invisible(self)
    },

    open = function() {
      editor_show_page(self$url, local = TRUE)
    },

    print = function(...) {
      file = private$view$file()
      what = if (nzchar(file)) cli::format_inline("{.file {file}}") else "a broadcast"
      status = if (!self$is_running()) "stopped" else if (self$is_connected()) "connected" else "reconnecting"
      cli::cli_text("{.strong livecode follower:} {what} @ {.url {self$url}} ({status})")
      invisible(self)
    }
  ),

  active = list(
    url = function() {
      private$view$url()
    }
  ),

  private = list(
    new_client = NULL,
    client = NULL,
    view = NULL,
    conn = NULL,
    port = NULL,
    dial_timeout = 30,
    gen = 0L,
    stopped = FALSE,
    have_hello = FALSE,
    splitter = NULL,
    retry_delay = 1,
    cancel_retry = NULL,
    reported = NULL,

    connect = function() {
      if (private$stopped)
        return(invisible())
      private$gen = private$gen + 1L
      gen = private$gen
      p = tryCatch(
        {
          if (is.null(private$client))
            private$client = private$new_client()
          private$client$dial_async(private$port, timeout = private$dial_timeout)
        },
        error = function(e) e
      )
      if (inherits(p, "error")) {
        private$disconnected(gen, p)
        return(invisible())
      }
      promises::then(
        p,
        onFulfilled = function(conn) {
          if (gen != private$gen || private$stopped) {
            tc_close_quietly(conn)
            return(invisible())
          }
          private$conn = conn
          self$reset_stream()
          private$read_next(gen)
        },
        onRejected = function(e) private$disconnected(gen, e)
      )
      invisible()
    },

    read_next = function(gen) {
      p = tryCatch(
        private$conn$read_async(65536L, timeout = tc_read_timeout),
        error = function(e) e
      )
      if (inherits(p, "error")) {
        private$disconnected(gen, p)
        return(invisible())
      }
      promises::then(
        p,
        onFulfilled = function(bytes) {
          if (gen != private$gen)
            return(invisible())
          if (is.null(bytes)) {
            private$disconnected(gen, NULL)
            return(invisible())
          }
          ok = tryCatch(
            {
              self$receive(bytes)
              TRUE
            },
            error = function(e) {
              private$disconnected(gen, e)
              FALSE
            }
          )
          if (ok)
            private$read_next(gen)
        },
        onRejected = function(e) private$disconnected(gen, e)
      )
      invisible()
    },

    handle_line = function(line) {
      x = tc_parse(line)
      if (!private$have_hello) {
        hello = tc_validate_hello(x)
        private$have_hello = TRUE
        private$retry_delay = tc_retry_min
        private$view$set_hello(hello)
        private$view$set_status("connected")
        if (!identical(private$reported, "connected")) {
          private$reported = "connected"
          what = if (nzchar(hello$file)) cli::format_inline("{.file {hello$file}}") else "the broadcast"
          cli::cli_bullets(c("v" = "Following {what} at {.url {self$url}}."))
        }
        return(invisible())
      }
      frame = tc_validate_frame(x)
      if (length(frame) > 0)
        private$view$update(frame)
      invisible()
    },

    close_client = function() {
      if (!is.null(private$client)) {
        try(private$client$close(), silent = TRUE)
        private$client = NULL
      }
    },

    close_conn = function() {
      if (!is.null(private$conn)) {
        tc_close_quietly(private$conn)
        private$conn = NULL
      }
      private$have_hello = FALSE
    },

    # Any failure, from a dial that times out to a line that is too long,
    # ends the connection and schedules another try.
    disconnected = function(gen, e) {
      if (gen != private$gen || private$stopped)
        return(invisible())
      private$gen = private$gen + 1L
      private$close_conn()
      private$close_client()
      private$view$set_status("reconnecting")

      reason = if (is.null(e)) "The broadcast closed the connection." else conditionMessage(e)
      if (!identical(private$reported, reason)) {
        private$reported = reason
        cli::cli_bullets(c("!" = "livecode: lost the broadcast, reconnecting."))
        cli::cli_bullets(c("i" = "{reason}"))
      }

      delay = private$retry_delay
      private$retry_delay = min(2 * delay, tc_retry_max)
      private$cancel_retry = later::later(function() {
        private$cancel_retry = NULL
        private$connect()
      }, delay)
      invisible()
    }
  )
)

register_follower = function(follower) {
  .globals$followers[[length(.globals$followers) + 1]] = follower
  invisible()
}

deregister_follower = function(follower) {
  match = vapply(.globals$followers, identical, logical(1), y = follower)
  .globals$followers = .globals$followers[!match]
  invisible(any(match))
}

#' Follow a tailcat broadcast
#'
#' @description
#' Shows a session broadcast with \code{serve_file(tailcat = TRUE)} on a
#' page served by this R session, from anywhere with an internet
#' connection. The page looks like the instructor's: the file highlighted,
#' the lines the instructor selects, and messages.
#'
#' @param address The tailcat address of the broadcast, a string starting
#'   with \code{"tc"}, as posted by the instructor.
#' @param port The tailcat port of the broadcast, \code{4000} unless the
#'   instructor chose another.
#' @param open_browser Should the page be opened: in Positron's Viewer pane,
#'   elsewhere in the system browser.
#' @param local_port Port of the local page on \code{127.0.0.1}, by default
#'   a random free port.
#'
#' @details
#' The page is served on \code{127.0.0.1} only and works while the R
#' session is idle at the console, or while the \pkg{later} event loop
#' otherwise runs. If the broadcast is interrupted, for example while the
#' instructor restarts it or the network drops, the page shows that it is
#' reconnecting and catches up on its own; the connection is retried after
#' 1 second, then 2, 4, and so on up to 30.
#'
#' Everything received is treated as text to display. Nothing is ever run,
#' sourced, or saved, the file name is used for display only, and messages
#' are shown as plain text. Copy code from the page yourself if you want to
#' run it.
#'
#' Anyone can start a broadcast, so only use an address the instructor
#' posted somewhere only they can edit, such as the course site.
#'
#' Needs the rtailcat package, which is not on CRAN; install it with
#' \code{pak::pak("rundel/rtailcat")}, which builds it from source and needs
#' a Go toolchain.
#'
#' @return
#' An invisible \code{LiveCodeFollower} R6 object with \code{$stop()},
#' \code{$open()}, and \code{$url}. \code{\link{stop_all}} stops followers
#' too.
#'
#' @examples
#' \dontrun{
#' f = livecode::follow("tc...")
#' f$stop()
#' }
#'
#' @seealso \code{\link{serve_file}}
#'
#' @export
follow = function(address, port = 4000, open_browser = TRUE, local_port = NULL) {
  tc_check_installed(c("rtailcat", "promises"), "Following a broadcast")

  if (!is_string(address) || !nzchar(trimws(address)))
    cli::cli_abort("{.arg address} must be a single tailcat address, as posted by the instructor.")
  address = trimws(address)

  ok = tryCatch(
    {
      rtailcat::tailcat_address_parse(address)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    cli::cli_abort(c(
      "{.arg address} is not a valid tailcat address.",
      "i" = "Copy the whole address the instructor posted, it starts with {.val tc}."
    ))
  }

  port = suppressWarnings(as.integer(port))
  if (length(port) != 1 || is.na(port) || port < 1L || port > 65535L)
    cli::cli_abort("{.arg port} must be a single port number between 1 and 65535.")

  rtailcat::TailcatClient$new(address)$close()
  new_client = function() rtailcat::TailcatClient$new(address)
  follower = lc_follower$new(new_client, port = port, local_port = local_port)
  register_follower(follower)
  cli::cli_bullets(c("i" = "Connecting to the broadcast, the page is at {.url {follower$url}}."))

  # Positron answers the request to show the page only while this call runs.
  if (open_browser && identical(editor_backend(), "positron"))
    follower$open()
  else if (open_browser)
    later::later(function() if (follower$is_running()) follower$open(), 0.5)

  invisible(follower)
}

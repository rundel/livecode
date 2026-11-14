# The follower's local page: an httpuv server on 127.0.0.1 that serves the
# same page as the instructor's server and relays validated frames to it
# over a websocket. It never takes input from the page.
#
# The page lives under a random token in its URL, and nothing is served
# without it. The token is what says that a request comes from this
# follower's own page: an IDE's Viewer may reach the page through a proxy,
# such as Posit Workbench's, whose address is shared by every user and
# session of the service, so the Host and Origin headers cannot say that.
# It also keeps out any other web page open in the browser, a DNS rebinding
# page included, and other users of the same computer.
follower_view = R6::R6Class(
  "livecode_follower_view",
  cloneable = FALSE,
  public = list(
    initialize = function(port = NULL) {
      host = "127.0.0.1"
      if (is.null(port))
        port = httpuv::randomPort(host = host)
      private$port = as.integer(port)
      private$state = list(
        file = "", lang = "none", content = "", selection = "", status = "waiting",
        route = tc_route_unknown()
      )
      private$websockets = new.env(parent = emptyenv())
      private$token = random_token()
      private$handle = httpuv::startServer(host, private$port, private$app())
    },

    url = function() {
      paste0("http://127.0.0.1:", private$port, private$root())
    },

    local_port = function() {
      private$port
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
      private$interval = hello$interval
      private$send(list(lang = hello$lang))
    },

    update = function(frame) {
      for (field in intersect(names(frame), c("content", "selection")))
        private$state[[field]] = frame[[field]]
      private$send(frame)
    },

    set_route = function(route) {
      if (identical(route, private$state$route))
        return(invisible(self))
      private$state$route = route
      private$send(list(route = route))
    },

    set_status = function(status) {
      if (identical(status, private$state$status))
        return(invisible(self))
      private$state$status = status
      private$send(list(status = status))
      private$beat()
    },

    stop = function() {
      if (self$is_running()) {
        close_websockets(private$websockets, list(status = "stopped"))
        httpuv::stopServer(private$handle)
      }
      private$beat()
      invisible(self)
    }
  ),

  private = list(
    handle = NULL,
    port = NULL,
    token = NULL,
    state = NULL,
    websockets = NULL,
    next_ws_id = 0L,
    interval = NULL,
    cancel_beat = NULL,

    # The broadcast sends changes only, so the page's progress circle, which
    # the instructor's server restarts at every update, is driven from here
    # at the broadcast's update interval while it is connected.
    beat = function() {
      if (!is.null(private$cancel_beat)) {
        private$cancel_beat()
        private$cancel_beat = NULL
      }
      if (is.null(private$interval) || !self$is_running() ||
          !identical(private$state$status, "connected"))
        return(invisible())

      private$send(list(interval = private$interval))
      private$cancel_beat = later::later(function() private$beat(), private$interval)
      invisible()
    },

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
      private$state[c("status", "lang", "content", "selection", "route")]
    },

    root = function() {
      paste0("/", private$token, "/")
    },

    static_paths = function() {
      paths = list(pkg_resource("resources"))
      names(paths) = paste0(private$root(), "web")
      paths
    },

    path_ok = function(req) {
      path = req[["PATH_INFO"]]
      is_string(path) && startsWith(path, private$root())
    },

    app = function() {
      refuse = function(status, text) {
        list(
          status = status,
          headers = list("Content-Type" = "text/plain; charset=UTF-8"),
          body = text
        )
      }

      list(
        call = function(req) {
          if (!private$path_ok(req))
            return(refuse(403L, "Forbidden"))
          if (!identical(req[["PATH_INFO"]], private$root()))
            return(refuse(404L, "Not found"))

          file = if (nzchar(private$state$file)) private$state$file else "livecode"
          list(
            status = 200L,
            headers = list("Content-Type" = "text/html; charset=UTF-8"),
            body = livecode_page(file, private$state$lang)
          )
        },

        onWSOpen = function(ws) {
          if (!private$path_ok(ws$request)) {
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

        staticPaths = private$static_paths()
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
                          dial_timeout = 30, address = NULL) {
      private$new_client = new_client
      private$address = address
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
      for (line in lines) {
        if (private$ended)
          break
        private$handle_line(line)
      }
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

    # Is this follower still following the broadcast at this address and
    # port? One that was stopped, or whose broadcast has ended, is not.
    follows = function(address, port) {
      self$is_running() && !private$ended &&
        identical(private$address, address) && identical(private$port, port)
    },

    # TRUE once the broadcast said that it stopped. The page stays up with
    # the last text it showed, and nothing is dialed any more.
    has_ended = function() {
      private$ended
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
      status = if (!self$is_running()) {
        "stopped"
      } else if (private$ended) {
        "ended"
      } else if (self$is_connected()) {
        "connected"
      } else {
        "reconnecting"
      }
      cli::cli_text("{.strong livecode follower:} {what} @ {.url {self$url}} ({status})")
      invisible(self)
    }
  ),

  active = list(
    url = function() {
      private$view$url()
    },

    # The port of the local page, which is how unfollow() tells followers
    # apart.
    local_port = function() {
      private$view$local_port()
    }
  ),

  private = list(
    new_client = NULL,
    address = NULL,
    client = NULL,
    view = NULL,
    conn = NULL,
    port = NULL,
    dial_timeout = 30,
    gen = 0L,
    stopped = FALSE,
    ended = FALSE,
    have_hello = FALSE,
    splitter = NULL,
    retry_delay = 1,
    cancel_retry = NULL,
    cancel_probe = NULL,
    reported = NULL,

    # Asks tailcat, every few seconds while connected, whether the
    # connection is direct or goes through a relay, for the page to show. A
    # discovery ping also makes tailcat look for a direct path.
    probe_route = function(gen) {
      private$cancel_probe = NULL
      if (gen != private$gen || is.null(private$client))
        return(invisible())

      again = function() {
        if (gen == private$gen) {
          private$cancel_probe = later::later(
            function() private$probe_route(gen), tc_path_interval
          )
        }
      }
      p = tryCatch(
        private$client$disco_ping_async(timeout = tc_path_timeout),
        error = function(e) NULL
      )
      if (is.null(p)) {
        again()
        return(invisible())
      }
      promises::then(
        p,
        onFulfilled = function(ping) {
          if (gen == private$gen)
            private$view$set_route(tc_route(ping))
          again()
        },
        onRejected = function(e) again()
      )
      invisible()
    },

    connect = function() {
      if (private$stopped || private$ended)
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
          private$probe_route(gen)
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
          if (ok && gen == private$gen)
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
          cli::cli_bullets(c(
            "v" = "Following {what} at {.url {self$url}}.",
            " " = "Run {.run livecode::unfollow()} to stop following."
          ))
        }
        return(invisible())
      }
      if (tc_is_bye(x)) {
        private$finish()
        return(invisible())
      }
      frame = tc_validate_frame(x)
      if (length(frame) > 0)
        private$view$update(frame)
      invisible()
    },

    # One line when the broadcast is lost and one more if it then cannot be
    # reached, instead of a report for every failed attempt.
    report_outage = function(e) {
      reason = tc_outage_reason(e)
      stage = private$reported
      if (is.null(stage)) {
        private$reported = "unreachable"
        cli::cli_bullets(c("!" = "Could not reach the broadcast, retrying: {reason}"))
      } else if (identical(stage, "connected")) {
        private$reported = "lost"
        cli::cli_bullets(c("!" = "Lost the broadcast, reconnecting: {reason}"))
      } else if (identical(stage, "lost")) {
        private$reported = "unreachable"
        cli::cli_bullets(c(
          "i" = "Still reconnecting: {reason}",
          " " = "This keeps trying on its own, run {.run livecode::unfollow()} to stop following."
        ))
      }
      invisible()
    },

    # The broadcast said goodbye: it was stopped, not cut off, so there is
    # nothing to reconnect to. A broadcast started again is a new session,
    # which follow() joins.
    finish = function() {
      private$ended = TRUE
      private$gen = private$gen + 1L
      if (!is.null(private$cancel_retry))
        private$cancel_retry()
      private$close_conn()
      private$close_client()
      private$view$set_status("finished")
      private$reported = "ended"
      cli::cli_bullets(c(
        "i" = "The broadcast has ended. The page keeps the last text it showed.",
        " " = "Run {.run livecode::unfollow()} to close the page."
      ))
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
      if (!is.null(private$cancel_probe)) {
        private$cancel_probe()
        private$cancel_probe = NULL
      }
      private$view$set_route(tc_route_unknown())
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

      private$report_outage(e)

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

# Why the broadcast was lost, as one short sentence: the first line of the
# error, without the advice tailcat adds for someone debugging a server.
tc_outage_reason = function(e) {
  if (is.null(e))
    return("the broadcast closed the connection.")
  line = strsplit(cli::ansi_strip(conditionMessage(e)), "\n", fixed = TRUE)[[1]]
  line = trimws(line[nzchar(trimws(line))])
  if (length(line) == 0)
    return("the connection failed.")
  line = line[1]
  line = paste0(tolower(substr(line, 1, 1)), substr(line, 2, nchar(line)))
  if (!grepl("[.!?]$", line))
    line = paste0(line, ".")
  line
}

register_follower = function(follower) {
  .globals$followers[[length(.globals$followers) + 1]] = follower
  invisible()
}

# The running follower of this address and port, if there is one.
find_follower = function(address, port) {
  for (follower in .globals$followers) {
    if (follower$follows(address, port))
      return(follower)
  }
  NULL
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
#' and the lines the instructor selects.
#'
#' @param address The tailcat address of the broadcast, a string starting
#'   with \code{"tc"}, as posted by the instructor.
#' @param port The tailcat port of the broadcast, \code{4000} unless the
#'   instructor chose another.
#' @param open_browser Should the page be opened: in the Viewer pane of
#'   RStudio or Positron, elsewhere in the system browser. Set
#'   \code{options(livecode.viewer = FALSE)} to always use the browser.
#' @param local_port Port of the local page on \code{127.0.0.1}, by default
#'   a random free port.
#'
#' @details
#' The page is served on \code{127.0.0.1} only, under a random token in its
#' address without which nothing is served, so only the page that
#' \code{follow()} opens can read the session, also when an IDE shows it
#' through a proxy such as Posit Workbench's. It works while the R
#' session is idle at the console, or while the \pkg{later} event loop
#' otherwise runs. If the broadcast is interrupted, for example while the
#' instructor restarts it or the network drops, the page shows that it is
#' reconnecting and catches up on its own; the connection is retried after
#' 1 second, then 2, 4, and so on up to 30. When the instructor stops the
#' broadcast, the page says that the session has ended, keeps the last text
#' it showed, and the connection is not retried; run \code{follow()} again
#' to follow a broadcast that is started again.
#'
#' Everything received is treated as text to display. Nothing is ever run,
#' sourced, or saved, and the file name is used for display only. Copy code from the page yourself if you want to
#' run it.
#'
#' Anyone can start a broadcast, so only use an address the instructor
#' posted somewhere only they can edit, such as the course site.
#'
#' Needs the rtailcat package, which is not on CRAN; install it with
#' \code{pak::pak("rundel/rtailcat")}, which builds it from source and needs
#' a Go toolchain.
#'
#' Calling \code{follow()} again with an address that is already being
#' followed does not start a second page: it opens the page of the running
#' follower and returns that follower. Once the broadcast has ended, or the
#' follower was stopped, \code{follow()} starts a new one.
#'
#' @section Stopping:
#' Following runs until it is stopped: \code{\link{unfollow}()} stops every
#' follower of the session, or the one whose page is on a given port, and
#' \code{$stop()} on the returned object stops that one. Either closes the connection to the broadcast and
#' the local page's server; the page then says that following has stopped.
#' \code{follow()} says so once it is connected, and again when the
#' broadcast ends, after which the local page stays up with the last text until it is
#' stopped. Closing the Viewer or the browser tab does not stop it.
#'
#' @return
#' An invisible \code{LiveCodeFollower} R6 object with \code{$stop()},
#' \code{$open()}, \code{$url}, and \code{$local_port}.
#'
#' @examples
#' \dontrun{
#' f = livecode::follow("tc...")
#' f$stop()
#' }
#'
#' @seealso \code{\link{unfollow}}, \code{\link{serve_file}}
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

  follower = find_follower(address, port)
  if (!is.null(follower)) {
    cli::cli_bullets(c(
      "i" = "Already following this broadcast at {.url {follower$url}}.",
      " " = "Run {.run livecode::unfollow()} to stop following."
    ))
  } else {
    rtailcat::TailcatClient$new(address)$close()
    new_client = function() rtailcat::TailcatClient$new(address)
    follower = lc_follower$new(
      new_client, port = port, local_port = local_port, address = address
    )
    register_follower(follower)
    cli::cli_bullets(c("i" = "Connecting to the broadcast\u2026"))
  }

  # Positron answers the request to show the page only while this call runs.
  if (open_browser && identical(editor_backend(), "positron"))
    follower$open()
  else if (open_browser)
    later::later(function() if (follower$is_running()) follower$open(), 0.5)

  invisible(follower)
}

#' Stop following tailcat broadcasts
#'
#' @description
#' Stops followers started with \code{\link{follow}()}: the connection to
#' the broadcast is closed and so is the server of the local page, which
#' then says that following has stopped.
#'
#' @param port The port of the local page of the follower to stop, the
#'   number after \code{127.0.0.1:} in the address \code{follow()} printed.
#'   \code{NULL} (the default) stops every follower of this R session.
#'
#' @return
#' The number of followers stopped, invisibly.
#'
#' @examples
#' \dontrun{
#' livecode::follow("tc...")
#' #> Following 'lecture.R' at <http://127.0.0.1:28664/...>.
#'
#' # Stop that one
#' livecode::unfollow(28664)
#'
#' # Stop them all
#' livecode::unfollow()
#' }
#'
#' @seealso \code{\link{follow}}, \code{\link{stop_all}}
#'
#' @export
unfollow = function(port = NULL) {
  followers = .globals$followers

  if (!is.null(port)) {
    if (!is.numeric(port) || length(port) != 1 || is.na(port) || port != trunc(port))
      cli::cli_abort("{.arg port} must be a single port number, or {.code NULL} to stop every follower.")

    ports = vapply(followers, function(f) f$local_port, integer(1))
    followers = followers[ports == port]
    if (length(followers) == 0) {
      hint = if (length(ports) > 0) {
        "The pages being followed are on {cli::qty(length(ports))}port{?s} {ports}."
      } else {
        "Nothing is being followed."
      }
      cli::cli_abort(c("No follower has its page on port {port}.", "i" = hint))
    }
  }

  if (length(followers) == 0) {
    cli::cli_bullets(c("i" = "Not following any broadcast."))
    return(invisible(0L))
  }

  for (follower in followers)
    follower$stop()
  invisible(length(followers))
}

# A stand-in for a tailcat connection that records what is done with it.
# `write` decides how writes end: "ok", "hang" (never), "fail" (the promise
# rejects), or "throw" (write_async itself errors, as on a closed
# connection). `chunks` are what reads return, in order, after which a read
# stays pending, or ends the stream with eof = TRUE.
fake_conn = function(write = "ok", chunks = list(), eof = FALSE) {
  conn = new.env()
  conn$written = list()
  conn$timeouts = numeric()
  conn$reads = 0L
  conn$closed = FALSE
  conn$chunks = chunks

  conn$write_async = function(data, timeout = Inf) {
    if (write == "throw")
      stop("the connection is closed")
    conn$written[[length(conn$written) + 1]] = data
    conn$timeouts = c(conn$timeouts, timeout)
    switch(
      write,
      ok = promises::promise_resolve(nchar(data, type = "bytes")),
      hang = promises::promise(function(resolve, reject) NULL),
      fail = promises::promise_reject(simpleError("timed out"))
    )
  }
  conn$write = function(...) {
    conn$written[[length(conn$written) + 1]] = list(...)[[1]]
    invisible(0L)
  }

  conn$read_async = function(n = 65536L, timeout = Inf, ...) {
    conn$reads = conn$reads + 1L
    if (length(conn$chunks) > 0) {
      chunk = conn$chunks[[1]]
      conn$chunks = conn$chunks[-1]
      return(promises::promise_resolve(chunk))
    }
    if (eof)
      return(promises::promise_resolve(NULL))
    promises::promise(function(resolve, reject) NULL)
  }
  conn$read = function(...) {
    conn$reads = conn$reads + 1L
    raw(0)
  }
  conn$read_all = conn$read
  conn$read_all_async = conn$read_async

  conn$close = function() {
    conn$closed = TRUE
    invisible(conn)
  }
  conn
}

# A stand-in for a tailcat client: each dial hands out the next connection,
# or rejects when there is none.
fake_client = function(conns = list()) {
  client = new.env()
  client$dials = 0L
  client$closed = FALSE
  client$conns = conns
  client$dial_async = function(port, timeout = 30, ...) {
    client$dials = client$dials + 1L
    if (length(client$conns) == 0)
      return(promises::promise_reject(simpleError("no broadcast at this address")))
    conn = client$conns[[1]]
    client$conns = client$conns[-1]
    promises::promise_resolve(conn)
  }
  client$close = function() {
    client$closed = TRUE
    invisible(client)
  }
  client
}

lines_bytes = function(...) {
  charToRaw(enc2utf8(paste0(vapply(list(...), tc_encode, character(1)), collapse = "")))
}

hello_bytes = function(file = "lecture.R", lang = "r") {
  lines_bytes(tc_hello(file, lang, 1))
}

local_follower = function(client, env = parent.frame()) {
  follower = suppressMessages(lc_follower$new(function() client, local_port = NULL))
  register_follower(follower)
  withr::defer(if (follower$is_running()) suppressMessages(follower$stop()), envir = env)
  follower
}

# A websocket client of a local page that sends the given Origin header
# (none when NULL).
origin_client = function(url, origin = NULL, env = parent.frame()) {
  client = new.env()
  client$frames = list()
  client$closed = FALSE
  headers = if (is.null(origin)) list() else list(Origin = origin)
  client$ws = websocket::WebSocket$new(
    sub("^http", "ws", url), headers = headers, autoConnect = FALSE, errorLogChannels = "none"
  )
  client$ws$onMessage(function(e) {
    client$frames[[length(client$frames) + 1]] = jsonlite::fromJSON(e$data, simplifyVector = FALSE)
  })
  client$ws$onClose(function(e) client$closed = TRUE)
  client$ws$connect()
  withr::defer(close_client(client), envir = env)
  pump(3, until = function() client$closed || length(client$frames) > 0)
  client
}

private_of = function(x) {
  x$.__enclos_env__$private
}

file_mode = function(path) {
  format(file.info(path)$mode)
}

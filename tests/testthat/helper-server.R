pump = function(seconds = 0.5, until = function() FALSE) {
  deadline = Sys.time() + seconds
  while (Sys.time() < deadline) {
    later::run_now(0.02)
    if (requireNamespace("curl", quietly = TRUE))
      curl::multi_run(timeout = 0.01, poll = TRUE)
    if (until())
      return(TRUE)
  }
  until()
}

# The server runs in this process, so a blocking request would deadlock;
# fetch asynchronously and keep the event loop running.
http_get = function(url, seconds = 5) {
  res = NULL
  curl::curl_fetch_multi(
    url,
    done = function(r) res <<- r,
    fail = function(msg) res <<- list(status_code = NA, error = msg)
  )
  pump(seconds, until = function() !is.null(res))
  res
}

close_client = function(client) {
  if (client$ws$readyState() %in% c(0L, 1L))
    try(client$ws$close(), silent = TRUE)
  pump(1, until = function() client$ws$readyState() == 3L)
}

ws_client = function(url, env = parent.frame()) {
  client = new.env()
  client$frames = list()
  client$open = FALSE
  client$ws = websocket::WebSocket$new(sub("^http", "ws", url), autoConnect = FALSE, errorLogChannels = "none")
  client$ws$onOpen(function(e) client$open = TRUE)
  client$ws$onMessage(function(e) {
    client$frames[[length(client$frames) + 1]] = jsonlite::fromJSON(e$data, simplifyVector = FALSE)
  })
  client$ws$connect()
  withr::defer(close_client(client), envir = env)
  pump(5, until = function() client$open && length(client$frames) > 0)
  client
}

contents = function(client) {
  unlist(lapply(client$frames, function(f) f[["content"]]))
}

local_test_file = function(text = "x = 1", env = parent.frame()) {
  path = withr::local_tempfile(fileext = ".R", .local_envir = env)
  writeLines(text, path)
  normalizePath(path)
}

# Bumps the mtime so the change is visible even on file systems with
# coarse timestamps.
touch_write = function(path, text) {
  old = file.mtime(path)
  writeLines(text, path)
  Sys.setFileTime(path, old + 10)
}

local_server = function(file, interval = 0.1, env = parent.frame()) {
  srv = suppressMessages(serve_file(
    file, ip = "127.0.0.1", interval = interval, open_browser = FALSE, auto_save = FALSE
  ))
  withr::defer(if (srv$is_running()) suppressMessages(srv$stop()), envir = env)
  srv
}

local_editor = function(backend, env = parent.frame()) {
  withr::local_options(livecode.editor = backend, .local_envir = env)
  editor_reset()
  withr::defer(editor_reset(), envir = env)
}

# A fake source editor: the context it reports and a log of the calls made.
fake_editor = function(path = NULL, id = NULL, contents = NULL, selection = list(), env = parent.frame()) {
  ed = new.env()
  ed$ctx = list(id = id, path = path, contents = contents, selection = selection)
  ed$calls = list()
  ed$reads = 0
  log = function(...) ed$calls[[length(ed$calls) + 1]] = list(...)

  testthat::local_mocked_bindings(
    rs_context = function() {
      ed$reads = ed$reads + 1
      ed$ctx
    },
    rs_save = function(id = NULL) log("save", id),
    rs_navigate = function(file) log("navigate", file),
    ps_execute_command = function(command) log("command", command),
    .env = env
  )
  ed
}

calls_of = function(ed, kind) {
  Filter(function(x) identical(x[[1]], kind), ed$calls)
}

sel_rows = function(first, last) {
  list(list(range = list(start = c(row = first, column = 1), end = c(row = last, column = 2)), text = ""))
}

local_lc_server = function(file, editor, auto_save = TRUE, file_id = NULL, env = parent.frame()) {
  srv = file_stream_server(
    "127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), file, file_id = file_id,
    interval = 10, auto_save = auto_save, editor = editor
  )
  withr::defer(srv$stop(), envir = env)
  srv
}

has_positron_callback = function() {
  positron_callback_name %in% getTaskCallbackNames()
}

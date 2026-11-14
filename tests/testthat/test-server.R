skip_if_not_installed("websocket")
skip_if_not_installed("curl")
skip_on_cran()

test_that("a server starts outside an IDE, serves the page, and stops", {
  f = local_test_file("x = 1")
  srv = local_server(f)

  expect_true(srv$is_running())
  expect_match(srv$url, "^http://127\\.0\\.0\\.1:[0-9]+$")

  res = http_get(srv$url)
  expect_equal(res$status_code, 200L)
  page = rawToChar(res$content)
  expect_match(page, "<title>file[^<]*\\.R</title>")
  expect_false(grepl(dirname(f), page, fixed = TRUE))

  expect_message(srv$stop(), "Stopped server")
  expect_false(srv$is_running())
})

test_that("the page title is the escaped basename", {
  dir = withr::local_tempdir()
  f = file.path(dir, "a<b>&c.R")
  writeLines("x = 1", f)
  srv = local_server(f)

  page = rawToChar(http_get(srv$url)$content)
  expect_match(page, "<title>a&lt;b&gt;&amp;c.R</title>", fixed = TRUE)
})

test_that("clients get the content on connect", {
  f = local_test_file("x = 1")
  srv = local_server(f)

  client = ws_client(srv$url)
  first = client$frames[[1]]
  expect_equal(first$content, "x = 1\n")
  expect_setequal(names(first), c("interval", "lang", "content"))
})

test_that("a late joiner does not swallow an update for existing clients", {
  f = local_test_file("x = 1")
  srv = local_server(f, interval = 0.3)

  a = ws_client(srv$url)
  pump(0.5)

  touch_write(f, "x = 2")
  b = ws_client(srv$url)

  pump(5, until = function() "x = 2\n" %in% contents(a) && "x = 2\n" %in% contents(b))
  expect_true("x = 2\n" %in% contents(a))
  expect_true("x = 2\n" %in% contents(b))
})

test_that("the frame records broadcast state separately from the cache", {
  f = local_test_file("x = 1")
  srv = file_stream_server("127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, interval = 10)
  on.exit(srv$stop())

  expect_null(srv$build_frame()$content)
  touch_write(f, "x = 2")
  # A connecting client reads the current state and must not mark the
  # change as sent.
  expect_equal(srv$connect_frame()$content, "x = 1\n")
  expect_equal(srv$build_frame()$content, "x = 2\n")
  expect_null(srv$build_frame()$content)
  expect_equal(srv$connect_frame()$content, "x = 2\n")
})

test_that("the tick survives a deleted file and keeps running", {
  f = local_test_file("x = 1")
  srv = local_server(f)
  client = ws_client(srv$url)

  unlink(f)
  n = length(client$frames)
  pump(1)
  expect_true(srv$is_running())
  expect_gt(length(client$frames), n)

  writeLines("x = 3", f)
  pump(3, until = function() "x = 3\n" %in% contents(client))
  expect_true("x = 3\n" %in% contents(client))
})

test_that("tick errors are reported once and the loop keeps going", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1", contents = c("x = 1", ""))
  srv = local_lc_server(f, "rstudio", file_id = "ID1", interval = 0.1)
  log = frame_log(srv)

  local_mocked_bindings(editor_read = function(...) stop("boom"))
  msgs = capture_messages(pump(1))
  expect_equal(sum(grepl("boom", msgs)), 1)
  expect_true(srv$is_running())

  touch_write(f, "x = 4")
  pump(3, until = function() "x = 4\n" %in% unlist(lapply(log$frames, `[[`, "content")))
  expect_true("x = 4\n" %in% unlist(lapply(log$frames, `[[`, "content")))

  local_mocked_bindings(editor_read = function(...) list(contents = "x = 5", selection = "", path = f))
  pump(3, until = function() "x = 5" %in% unlist(lapply(log$frames, `[[`, "content")))
  expect_true("x = 5" %in% unlist(lapply(log$frames, `[[`, "content")))
})

test_that("servers are registered, listed, and stopped by stop_all", {
  f = local_test_file("x = 1")
  a = local_server(f)
  b = local_server(f)

  servers = list_servers()
  expect_true(any(vapply(servers, identical, logical(1), y = a)))
  expect_true(any(vapply(servers, identical, logical(1), y = b)))

  suppressMessages(a$stop())
  expect_false(any(vapply(list_servers(), identical, logical(1), y = a)))

  expect_message(a$stop(), "not running")

  suppressMessages(stop_all())
  expect_false(b$is_running())
  expect_length(list_servers(), 0)
})

test_that("restart serves again on the same url", {
  f = local_test_file("x = 1")
  srv = local_server(f)
  url = srv$url
  suppressMessages(srv$restart())
  expect_true(srv$is_running())
  expect_equal(srv$url, url)
  expect_equal(http_get(srv$url)$status_code, 200L)
  expect_length(list_servers(), 1)
})

test_that("stop tells connected pages that the session has ended", {
  f = local_test_file("x = 1")
  srv = local_server(f)
  client = ws_client(srv$url)

  suppressMessages(srv$stop())
  pump(1, until = function() "ended" %in% unlist(lapply(client$frames, function(fr) fr$status)))
  expect_true("ended" %in% unlist(lapply(client$frames, function(fr) fr$status)))
})

test_that("invalid arguments give clear errors", {
  f = local_test_file("x = 1")
  expect_error(serve_file(f, ip = "::1", open_browser = FALSE), "IPv6")
  expect_error(serve_file(f, ip = "300.1.1.1", open_browser = FALSE), "Invalid ip")
  expect_error(serve_file(f, ip = "127.0.0.1", port = 80, open_browser = FALSE), "Invalid port")
  expect_error(serve_file(f, ip = "127.0.0.1", interval = 0, open_browser = FALSE), "interval")
  expect_error(serve_file(tempfile(), ip = "127.0.0.1", open_browser = FALSE), "Unable to locate")
  expect_error(serve_file(f, ip = "127.0.0.1", open_browser = FALSE, tunnel = TRUE), "unused argument")
  expect_length(list_servers(), 0)
})

test_that("stopping the server tells the page that the session has ended", {
  local_editor("none")
  f = local_test_file()
  srv = local_server(f, interval = 10)
  client = ws_client(srv$url)
  closed = FALSE
  client$ws$onClose(function(e) closed <<- TRUE)

  suppressMessages(srv$stop())
  pump(2, until = function() closed)
  expect_true(closed)
  expect_equal(client$frames[[length(client$frames)]], list(status = "ended"))
})

test_that("local_only serves on 127.0.0.1 and is only allowed with tailcat", {
  local_editor("none")
  f = local_test_file()

  expect_error(
    serve_file(f, local_only = TRUE, open_browser = FALSE),
    "needs a tailcat broadcast"
  )
  expect_error(
    serve_file(f, local_only = NA, open_browser = FALSE),
    "local_only"
  )
  testthat::local_mocked_bindings(tc_check_installed = function(...) invisible(TRUE))
  expect_error(
    serve_file(f, local_only = TRUE, tailcat = TRUE, ip = "127.0.0.1", open_browser = FALSE),
    "not both"
  )
  expect_length(list_servers(), 0)
})

test_that("a local_only server is served on 127.0.0.1 only", {
  local_editor("none")
  f = local_test_file()
  # The broadcast itself needs the network; the server is what is tested.
  fake_broadcast = list(new = function(...) list(
    send = function(frame) invisible(),
    is_running = function() TRUE,
    address = function() "tcFAKE",
    n_clients = function() 0L,
    stop = function(...) invisible()
  ))
  testthat::local_mocked_bindings(
    tc_check_installed = function(...) invisible(TRUE),
    tc_broadcast = fake_broadcast
  )

  expect_message(
    srv <- serve_file(f, local_only = TRUE, tailcat = TRUE, open_browser = FALSE, interval = 10),
    "this computer only"
  )
  withr::defer(if (srv$is_running()) suppressMessages(srv$stop()))
  expect_equal(srv$tailcat_address, "tcFAKE")
  expect_match(srv$url, "^http://127\\.0\\.0\\.1:[0-9]+$")

  client = ws_client(srv$url)
  expect_equal(client$frames[[1]]$content, readr::read_file(f))
})

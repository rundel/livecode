# End-to-end tests over real tailcat, through rtailcat's default (public)
# relays. They need the network, so they only run with
# LIVECODE_TEST_TAILCAT=1. The broadcast worker loads the same livecode as
# the tests: the source tree under devtools::test() (through pkgload),
# otherwise the installed package.

skip_tailcat_e2e = function() {
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("LIVECODE_TEST_TAILCAT"), "1"))
    testthat::skip("LIVECODE_TEST_TAILCAT is not set to 1")
  for (pkg in c("rtailcat", "callr", "promises", "ps", "websocket", "curl"))
    testthat::skip_if_not_installed(pkg)
  if (!is.null(tc_dev_path()))
    testthat::skip_if_not_installed("pkgload")
}

broadcast_of = function(srv) {
  private_of(srv)$tailcat
}

local_tailcat_server = function(file, tailcat = TRUE, env = parent.frame()) {
  srv = suppressMessages(serve_file(
    file, ip = "127.0.0.1", interval = 0.2, open_browser = FALSE, auto_save = FALSE,
    tailcat = tailcat
  ))
  withr::defer(if (srv$is_running()) suppressMessages(srv$stop()), envir = env)
  srv
}

local_follow = function(address, env = parent.frame()) {
  fl = suppressMessages(follow(address, open_browser = FALSE))
  withr::defer(if (fl$is_running()) suppressMessages(fl$stop()), envir = env)
  suppressMessages(pump(60, until = function() fl$is_connected()))
  fl
}

worker_alive = function(pid) {
  !is.null(pid) && isTRUE(tryCatch(ps::ps_is_running(ps::ps_handle(pid)), error = function(e) FALSE))
}

statuses = function(client) {
  unlist(lapply(client$frames, function(f) f$status))
}

last_status = function(client) {
  s = statuses(client)
  if (length(s) == 0) NA_character_ else s[length(s)]
}

message_texts = function(client) {
  unlist(lapply(client$frames, function(f) lapply(f$messages, function(m) m$text)))
}

test_that("a follower sees the file, its updates, and messages as plain text", {
  skip_tailcat_e2e()
  f = local_test_file(c("x = 1", "y = x + 1"))
  keys = rtailcat::tailcat_keygen()
  srv = local_tailcat_server(f, tailcat = keys)
  address = srv$tailcat_address
  expect_true(is_string(address) && startsWith(address, "tc"))

  fl = local_follow(address)
  expect_true(fl$is_connected())
  page = ws_client(fl$url)
  expect_equal(page$frames[[1]]$content, "x = 1\ny = x + 1\n")
  expect_equal(page$frames[[1]]$lang, "r")
  expect_match(rawToChar(http_get(fl$url)$content), "data-message-mode=\"text\"", fixed = TRUE)

  touch_write(f, "x = 2")
  pump(15, until = function() "x = 2\n" %in% contents(page))
  expect_true("x = 2\n" %in% contents(page))

  srv$send_msg("**bold** <b>raw</b>", type = "warning")
  pump(15, until = function() "**bold** <b>raw</b>" %in% message_texts(page))
  expect_true("**bold** <b>raw</b>" %in% message_texts(page))

  # The address never travels in the broadcast, and the files that know it
  # are readable by the owner only.
  dir = private_of(broadcast_of(srv))$dir
  seen = c(
    vapply(page$frames, function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE)), ""),
    readLines(file.path(dir, "worker.log"), warn = FALSE)
  )
  expect_false(any(grepl(address, seen, fixed = TRUE)))
  if (.Platform$OS.type != "windows") {
    expect_equal(file_mode(dir), "700")
    expect_equal(file_mode(file.path(dir, "status.json")), "600")
  }

  # The page learns whether the connection is direct or relayed.
  routes = function() Filter(Negate(is.null), lapply(page$frames, function(x) x$route$kind))
  pump(30, until = function() any(routes() %in% c("direct", "relay")))
  expect_true(any(routes() %in% c("direct", "relay")))

  # A broadcast that is cut off, here by killing its process, leaves the
  # page reconnecting; starting it again with the same keys brings the
  # follower back with the full state.
  pid = broadcast_of(srv)$worker_pid()
  tools::pskill(pid, tools::SIGKILL)
  suppressMessages(pump(60, until = function() identical(last_status(page), "reconnecting")))
  expect_false(worker_alive(pid))
  expect_equal(last_status(page), "reconnecting")
  expect_false(fl$is_connected())

  touch_write(f, "x = 3")
  suppressMessages(srv$start_tailcat(key = keys))
  expect_true(identical(srv$tailcat_address, address))
  suppressMessages(pump(90, until = function() fl$is_connected() && "x = 3\n" %in% contents(page)))
  expect_true(fl$is_connected())
  expect_equal(last_status(page), "connected")
  expect_true("x = 3\n" %in% contents(page))

  # Stopping the server stops the broadcast process, and the follower is
  # told that the session is over instead of being left to reconnect.
  pid = broadcast_of(srv)$worker_pid()
  suppressMessages({
    srv$stop()
    pump(30, until = function() fl$has_ended())
  })
  expect_false(worker_alive(pid))
  expect_false(dir.exists(dir))
  expect_null(srv$tailcat_address)
  expect_true(fl$has_ended())
  expect_false(fl$is_connected())
  expect_true(fl$is_running())
  pump(5, until = function() identical(last_status(page), "ended"))
  expect_equal(last_status(page), "ended")

  suppressMessages(stop_all())
  expect_false(fl$is_running())
})

test_that("a connection that never reads is dropped without holding up a follower", {
  skip_tailcat_e2e()
  f = local_test_file("x = 1")
  srv = local_tailcat_server(f)
  address = srv$tailcat_address
  fl = local_follow(address)
  page = ws_client(fl$url)

  # Dials and never reads.
  cl = rtailcat::TailcatClient$new(address)
  withr::defer(cl$close())
  stuck = cl$dial(tc_default_port, timeout = 30)
  pump(15, until = function() broadcast_of(srv)$n_clients() == 2)
  expect_equal(broadcast_of(srv)$n_clients(), 2)

  big = function(i) c(paste0("# version ", i), rep(strrep("z", 1000), 3000))
  for (i in 1:4) {
    touch_write(f, big(i))
    pump(1.5)
  }
  want = paste0(paste(big(4), collapse = "\n"), "\n")
  suppressMessages(pump(60, until = function() {
    fl$is_connected() && identical(utils::tail(contents(page), 1), want) &&
      broadcast_of(srv)$status()$dropped >= 1
  }))

  expect_gte(broadcast_of(srv)$status()$dropped, 1)
  expect_true(identical(utils::tail(contents(page), 1), want))
  expect_true(fl$is_connected())
  expect_equal(broadcast_of(srv)$n_clients(), 1)
})

test_that("a raw client gets the hello and the full state, and no address", {
  skip_tailcat_e2e()
  f = local_test_file("secret = 42")
  srv = local_tailcat_server(f)
  address = srv$tailcat_address

  cl = rtailcat::TailcatClient$new(address)
  withr::defer(cl$close())
  conn = cl$dial(tc_default_port, timeout = 30)
  bytes = raw(0)
  deadline = Sys.time() + 10
  split = tc_line_splitter()
  lines = character()
  while (length(lines) < 2 && Sys.time() < deadline) {
    chunk = tryCatch(conn$read(timeout = 2), error = function(e) raw(0))
    if (is.null(chunk)) break
    lines = c(lines, split(chunk))
  }
  conn$close()

  expect_gte(length(lines), 2)
  expect_false(any(grepl(address, lines, fixed = TRUE)))
  hello = tc_parse(lines[1])
  expect_equal(hello$livecode, 1L)
  expect_equal(hello$file, basename(f))
  state = tc_parse(lines[2])
  expect_equal(state$content, "secret = 42\n")
  expect_match(state$messages[[1]]$text, "Welcome")
  expect_false(grepl("http", state$messages[[1]]$text))
})

test_that("tailcat_keys gives a stable address and leaves nothing running", {
  skip_tailcat_e2e()
  path = file.path(withr::local_tempdir(), "class.rds")
  keys = tailcat_keys(path)
  if (.Platform$OS.type != "windows")
    expect_equal(file_mode(path), "600")
  again = tailcat_keys(path)
  expect_true(identical(keys$address, again$address))
  expect_false(any(grepl(keys$address, utils::capture.output(print(keys)), fixed = TRUE)))

  f = local_test_file("x = 1")
  msgs = capture_messages({
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE, auto_save = FALSE, tailcat = keys)
  })
  withr::defer(suppressMessages(srv$stop()))
  expect_true(identical(srv$tailcat_address, keys$address))
  expect_false(any(grepl("differs", msgs)))
})

test_that("the broadcast process exits when its parent dies", {
  skip_tailcat_e2e()
  f = local_test_file("x = 1")
  pid_file = tempfile()
  parent = callr::r_bg(
    function(pkg_path, file, pid_file) {
      if (!is.null(pkg_path))
        pkgload::load_all(pkg_path, quiet = TRUE, helpers = FALSE, export_all = FALSE)
      ns = asNamespace("livecode")
      b = ns$tc_broadcast$new(file, "r", 1)
      writeLines(as.character(b$worker_pid()), pid_file)
      Sys.sleep(600)
    },
    args = list(pkg_path = tc_dev_path(), file = f, pid_file = pid_file)
  )
  withr::defer(parent$kill())

  deadline = Sys.time() + 90
  while (!file.exists(pid_file) && parent$is_alive() && Sys.time() < deadline)
    Sys.sleep(0.2)
  skip_if_not(file.exists(pid_file), "the broadcast did not start in the child")
  pid = as.integer(readLines(pid_file))
  expect_true(worker_alive(pid))

  parent$kill()
  deadline = Sys.time() + 10
  while (worker_alive(pid) && Sys.time() < deadline)
    Sys.sleep(0.2)
  expect_false(worker_alive(pid))
})

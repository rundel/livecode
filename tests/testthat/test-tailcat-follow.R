skip_if_not_installed("promises")
skip_if_not_installed("websocket")
skip_if_not_installed("curl")
skip_on_cran()

# Names of the functions a piece of code calls, not every symbol in it.
called_functions = function(expr) {
  if (!is.call(expr))
    return(character())
  fn = expr[[1]]
  here = if (is.name(fn)) as.character(fn) else if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[3]]) else character()
  c(here, unlist(lapply(as.list(expr), called_functions)))
}

fetch_with_host = function(url, host) {
  h = curl::new_handle()
  curl::handle_setheaders(h, Host = host)
  res = NULL
  curl::curl_fetch_multi(url, handle = h, done = function(r) res <<- r, fail = function(m) res <<- list(status_code = NA))
  pump(5, until = function() !is.null(res))
  res
}

test_that("the local page is served on 127.0.0.1 in plain-text message mode", {
  view = follower_view$new()
  on.exit(view$stop())
  expect_match(view$url(), "^http://127\\.0\\.0\\.1:[0-9]+$")

  res = http_get(view$url())
  expect_equal(res$status_code, 200L)
  page = rawToChar(res$content)
  expect_match(page, "data-message-mode=\"text\"", fixed = TRUE)

  instructor = livecode_page("a.R")
  expect_match(instructor, "data-message-mode=\"html\"", fixed = TRUE)
})

test_that("the local page is not reachable on other interfaces", {
  ips = network_interfaces()[["ip"]]
  ips = ips[!startsWith(ips, "127.")]
  skip_if(length(ips) == 0, "no non-loopback interface")

  view = follower_view$new()
  on.exit(view$stop())
  port = sub("^.*:", "", view$url())
  res = http_get(paste0("http://", ips[1], ":", port), seconds = 3)
  expect_true(is.null(res) || is.na(res$status_code))
})

test_that("requests for another host are refused", {
  view = follower_view$new()
  on.exit(view$stop())
  port = sub("^.*:", "", view$url())

  expect_equal(fetch_with_host(view$url(), "evil.example")$status_code, 403L)
  expect_equal(fetch_with_host(view$url(), paste0("localhost:", port))$status_code, 200L)
})

test_that("websockets with a foreign Origin are closed without data", {
  view = follower_view$new()
  on.exit(view$stop())
  view$update(list(content = "secret lecture"))

  evil = origin_client(view$url(), origin = "http://evil.example")
  pump(1, until = function() evil$closed)
  expect_true(evil$closed)
  expect_length(evil$frames, 0)

  sneaky = origin_client(view$url(), origin = paste0(view$url(), ".evil.example"))
  pump(1, until = function() sneaky$closed)
  expect_length(sneaky$frames, 0)

  own = origin_client(view$url(), origin = view$url())
  expect_false(own$closed)
  expect_equal(own$frames[[1]]$content, "secret lecture")

  none = origin_client(view$url())
  expect_equal(none$frames[[1]]$content, "secret lecture")
})

test_that("a follower relays validated frames and drops everything else", {
  conn = fake_conn()
  follower = local_follower(fake_client(list(conn)))
  pump(0.3)

  page = origin_client(follower$url, origin = follower$url)
  expect_equal(page$frames[[1]]$status, "waiting")

  suppressMessages(follower$receive(hello_bytes("../../x/lecture.qmd", "qmd")))
  follower$receive(lines_bytes(
    list(content = "x = 1\n", selection = "1", eval = "system('touch x')", interval = 1),
    list(selection = "1;alert(1)"),
    list(messages = list(list(text = "<b>hi</b>", type = "success", layout = "center"))),
    list(ping = TRUE)
  ))
  pump(0.5, until = function() length(page$frames) >= 5)

  frames = page$frames[-1]
  expect_equal(frames[[1]], list(lang = "qmd"))
  expect_equal(frames[[2]], list(status = "connected"))
  expect_equal(frames[[3]], list(content = "x = 1\n", selection = "1"))
  expect_equal(frames[[4]]$messages[[1]]$text, "<b>hi</b>")
  expect_equal(frames[[4]]$messages[[1]]$layout, "topRight")
  expect_length(frames, 4)

  expect_equal(conn$reads > 0, TRUE)
  expect_length(conn$written, 0)

  late = origin_client(follower$url, origin = follower$url)
  expect_equal(late$frames[[1]][c("status", "lang", "content", "selection")], list(
    status = "connected", lang = "qmd", content = "x = 1\n", selection = "1"
  ))

  page_html = rawToChar(http_get(follower$url)$content)
  expect_match(page_html, "<title>lecture.qmd</title>", fixed = TRUE)
})

test_that("received content is shown, never run or written", {
  marker = file.path(withr::local_tempdir(), "pwned")
  code = paste0("file.create(", deparse(marker), ")")
  follower = local_follower(fake_client(list(fake_conn())))
  suppressMessages(follower$receive(hello_bytes()))
  follower$receive(lines_bytes(list(content = code, messages = list(list(text = code)))))
  pump(0.3)
  expect_false(file.exists(marker))

  forbidden = c(
    "eval", "evalq", "parse", "source", "sys.source", "str2lang", "str2expression",
    "system", "system2", "shell", "writeLines", "writeBin", "writeChar", "write",
    "cat", "sink", "saveRDS", "save", "file", "file.create", "do.call", "match.fun", "get"
  )
  funs = c(
    lc_follower$public_methods, lc_follower$private_methods,
    follower_view$public_methods, follower_view$private_methods,
    list(tc_line_splitter, tc_line_text, tc_parse, tc_validate_hello, tc_validate_frame, tc_validate_message, tc_clean_name)
  )
  used = unique(unlist(lapply(funs, function(f) called_functions(body(f)))))
  expect_equal(intersect(used, forbidden), character())
})

test_that("a line over the cap closes the connection and the page shows reconnecting", {
  chunk = as.raw(rep(120L, 65536))
  conn = fake_conn(chunks = rep(list(chunk), 81))
  follower = local_follower(fake_client(list(conn)))
  page = origin_client(follower$url, origin = follower$url)

  suppressMessages(expect_message(
    pump(5, until = function() conn$closed),
    "reconnecting"
  ))
  expect_true(conn$closed)
  pump(0.3)
  statuses = unlist(lapply(page$frames, function(f) f$status))
  expect_equal(utils::tail(statuses, 1), "reconnecting")
  expect_false(follower$is_connected())
})

test_that("a stream that does not start with a hello is dropped", {
  conn = fake_conn(chunks = list(lines_bytes(list(content = "x"))))
  follower = local_follower(fake_client(list(conn)))
  suppressMessages(pump(2, until = function() conn$closed))
  expect_true(conn$closed)
})

test_that("failed dials are retried with backoff and a new dial gets the full state", {
  client = fake_client()
  follower = local_follower(client)
  page = suppressMessages(origin_client(follower$url, origin = follower$url))
  priv = private_of(follower)

  suppressMessages(pump(0.3))
  expect_equal(client$dials, 1L)
  expect_equal(priv$retry_delay, 2)
  pump(0.5)
  expect_equal(utils::tail(unlist(lapply(page$frames, function(f) f$status)), 1), "reconnecting")

  suppressMessages(pump(1.2, until = function() client$dials == 2L))
  suppressMessages(pump(0.2))
  expect_equal(client$dials, 2L)
  expect_equal(priv$retry_delay, 4)

  client$conns = list(fake_conn(chunks = list(
    c(hello_bytes(), lines_bytes(list(content = "full state", selection = "")))
  )))
  suppressMessages(pump(3, until = function() follower$is_connected()))
  expect_true(follower$is_connected())
  expect_equal(priv$retry_delay, 1)
  pump(0.5, until = function() "full state" %in% contents(page))
  expect_true("full state" %in% contents(page))
})

test_that("the end of the stream leads to a reconnect", {
  first = fake_conn(chunks = list(hello_bytes()), eof = TRUE)
  second = fake_conn(chunks = list(hello_bytes()))
  client = fake_client(list(first, second))
  follower = local_follower(client)

  suppressMessages(pump(3, until = function() client$dials == 2L && follower$is_connected()))
  expect_true(first$closed)
  expect_equal(client$dials, 2L)
  expect_true(follower$is_connected())
})

test_that("followers are registered and stopped by stop_all", {
  client = fake_client(list(fake_conn()))
  follower = local_follower(client)
  expect_true(any(vapply(.globals$followers, identical, logical(1), y = follower)))

  expect_message(stop_all(), "Stopped following")
  expect_false(follower$is_running())
  expect_true(client$closed)
  expect_length(.globals$followers, 0)
  expect_equal(http_get(follower$url, seconds = 2)$status_code, NA)
})

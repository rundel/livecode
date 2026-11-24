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

view_base = function(view) {
  sub("^(http://[^/]+)/.*$", "\\1", view$url())
}

test_that("the local page is served on 127.0.0.1", {
  view = follower_view$new()
  on.exit(view$stop())
  expect_match(view$url(), "^http://127\\.0\\.0\\.1:[0-9]+/[0-9a-f]{32}/$")

  res = http_get(view$url())
  expect_equal(res$status_code, 200L)
  page = rawToChar(res$content)
  expect_match(page, "id=\"livecode-view\"", fixed = TRUE)
  expect_match(page, "<meta name=\"referrer\" content=\"no-referrer\">", fixed = TRUE)

  # The page's files are found next to it, under the token.
  expect_equal(http_get(paste0(view$url(), "web/livecode/livecode.js"))$status_code, 200L)
})

test_that("the local page is not reachable on other interfaces", {
  ips = network_interfaces()[["ip"]]
  ips = ips[!startsWith(ips, "127.")]
  skip_if(length(ips) == 0, "no non-loopback interface")

  view = follower_view$new()
  on.exit(view$stop())
  url = sub("127.0.0.1", ips[1], view$url(), fixed = TRUE)
  res = http_get(url, seconds = 3)
  expect_true(is.null(res) || is.na(res$status_code))
})

test_that("every page has a token of its own, from the system's random source", {
  set.seed(1)
  first = random_token()
  set.seed(1)
  second = random_token()
  expect_match(first, "^[0-9a-f]{32}$")
  expect_false(identical(first, second))

  a = follower_view$new()
  on.exit(a$stop())
  b = follower_view$new()
  on.exit(b$stop(), add = TRUE)
  expect_false(identical(sub("^.*:[0-9]+", "", a$url()), sub("^.*:[0-9]+", "", b$url())))
})

test_that("nothing is served without the page's token", {
  view = follower_view$new()
  on.exit(view$stop())
  base = view_base(view)
  wrong = paste0(strrep("0", 32), "/")

  for (path in c("/", paste0("/", wrong), "/web/livecode/livecode.js", paste0("/", wrong, "web/livecode/livecode.js")))
    expect_true(http_get(paste0(base, path))$status_code %in% c(403L, 404L), info = path)

  # The token as a prefix of a longer name, or without its slash, is not it.
  token = sub("^.*/([0-9a-f]{32})/$", "\\1", view$url())
  expect_equal(http_get(paste0(base, "/", token))$status_code, 403L)
  expect_equal(http_get(paste0(base, "/", token, "0/"))$status_code, 403L)
  expect_equal(http_get(paste0(view$url(), "other"))$status_code, 404L)
})

test_that("the page is served to a proxy's address when the token is right", {
  view = follower_view$new()
  on.exit(view$stop())
  view$update(list(content = "secret lecture"))

  expect_equal(fetch_with_host(view$url(), "workbench.example")$status_code, 200L)
  expect_equal(fetch_with_host(paste0(view_base(view), "/"), "workbench.example")$status_code, 403L)

  proxied = origin_client(view$url(), origin = "https://workbench.example")
  expect_equal(proxied$frames[[1]]$content, "secret lecture")
})

test_that("websockets without the token are closed without data, whatever their Origin", {
  view = follower_view$new()
  on.exit(view$stop())
  view$update(list(content = "secret lecture"))
  base = view_base(view)

  for (url in c(base, paste0(base, "/"), paste0(base, "/", strrep("0", 32), "/"))) {
    for (origin in list(NULL, base, "https://workbench.example")) {
      evil = origin_client(url, origin = origin)
      pump(1, until = function() evil$closed)
      expect_true(evil$closed)
      expect_length(evil$frames, 0)
    }
  }

  own = origin_client(view$url(), origin = base)
  expect_false(own$closed)
  expect_equal(own$frames[[1]]$content, "secret lecture")
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
  expect_equal(frames[[3]], list(interval = 1))
  expect_equal(frames[[4]], list(content = "x = 1\n", selection = "1"))
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
    list(tc_line_splitter, tc_line_text, tc_parse, tc_validate_hello, tc_validate_frame, tc_clean_name)
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
    "Could not reach the broadcast, retrying: a line from the broadcast"
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

test_that("the page's progress circle is driven at the broadcast's interval while connected", {
  follower = local_follower(fake_client(list(fake_conn())))
  pump(0.3)
  page = origin_client(follower$url, origin = follower$url)
  beats = function() sum(vapply(page$frames, function(f) identical(f, list(interval = 0.2)), logical(1)))

  suppressMessages(follower$receive(lines_bytes(tc_hello("lecture.R", "r", 0.2))))
  pump(2, until = function() beats() >= 3)
  expect_gte(beats(), 3)

  suppressMessages(follower$receive(lines_bytes(tc_bye())))
  pump(0.3)
  seen = beats()
  pump(0.6)
  expect_equal(beats(), seen)
})

test_that("a bye ends the session: the page is told, and nothing is dialed again", {
  conn = fake_conn()
  client = fake_client(list(conn, fake_conn()))
  follower = local_follower(client)
  pump(0.3)
  page = origin_client(follower$url, origin = follower$url)

  expect_message(
    follower$receive(c(
      hello_bytes(),
      lines_bytes(list(content = "x = 1\n"), tc_bye(), list(content = "after the end"))
    )),
    "The broadcast has ended"
  )
  expect_true(follower$has_ended())
  expect_false(follower$is_connected())
  expect_true(follower$is_running())
  expect_true(conn$closed)
  expect_true(client$closed)

  pump(1, until = function() identical(page$frames[[length(page$frames)]], list(status = "finished")))
  expect_equal(page$frames[[length(page$frames)]], list(status = "finished"))
  expect_false("after the end" %in% contents(page))

  pump(1.5)
  expect_equal(client$dials, 1L)
  expect_false(page$closed)

  late = origin_client(follower$url, origin = follower$url)
  expect_equal(late$frames[[1]]$status, "finished")
  expect_equal(late$frames[[1]]$content, "x = 1\n")
  expect_message(print(follower), "ended")
})

test_that("stopping a follower tells its page that the session has ended", {
  follower = local_follower(fake_client(list(fake_conn())))
  page = origin_client(follower$url, origin = follower$url)

  suppressMessages(follower$stop())
  pump(1, until = function() page$closed)
  expect_true(page$closed)
  expect_equal(page$frames[[length(page$frames)]], list(status = "stopped"))
})

test_that("the route of the connection is read from a discovery ping", {
  direct = list(direct = TRUE, endpoint = "192.0.2.7:41641", relay_region_code = "nyc")
  expect_equal(tc_route(direct), list(kind = "direct", relay = ""))
  relayed = list(direct = FALSE, endpoint = NA_character_, relay_region_code = "nyc")
  expect_equal(tc_route(relayed), list(kind = "relay", relay = "nyc"))
  relayed$relay_region_code = "<b>x</b>"
  expect_equal(tc_route(relayed), list(kind = "relay", relay = ""))
  expect_equal(tc_route(list()), list(kind = "relay", relay = ""))
})

test_that("the page is told whether the connection is direct or relayed, until it drops", {
  conn = fake_conn()
  client = fake_client(list(conn))
  client$pings = 0L
  client$disco_ping_async = function(timeout = 30) {
    client$pings = client$pings + 1L
    promises::promise_resolve(list(
      latency = 0.01, direct = FALSE, endpoint = NA_character_, relay_region_id = 1, relay_region_code = "nyc"
    ))
  }
  follower = local_follower(client)
  pump(1, until = function() client$pings > 0)
  expect_equal(client$pings, 1L)

  page = origin_client(follower$url, origin = follower$url)
  expect_equal(page$frames[[1]]$route, list(kind = "relay", relay = "nyc"))

  suppressMessages(follower$receive(c(hello_bytes(), lines_bytes(tc_bye()))))
  pump(1, until = function() identical(page$frames[[length(page$frames)]], list(status = "finished")))
  routes = Filter(Negate(is.null), lapply(page$frames[-1], function(f) f$route))
  expect_equal(routes, list(list(kind = "", relay = "")))
})

test_that("an outage is reported in one line, and once more if it lasts", {
  expect_equal(tc_outage_reason(NULL), "the broadcast closed the connection.")
  timeout = simpleError("Timed out after 30 seconds trying to dial port 4000.\nℹ tailcat: context deadline exceeded\nℹ A server that is offline does not answer.")
  expect_equal(tc_outage_reason(timeout), "timed out after 30 seconds trying to dial port 4000.")
  expect_equal(tc_outage_reason(simpleError("")), "the connection failed.")

  first = fake_conn(chunks = list(hello_bytes()), eof = TRUE)
  client = fake_client(list(first))
  msgs = character()
  withCallingHandlers(
    {
      follower = lc_follower$new(function() client)
      withr::defer(suppressMessages(follower$stop()))
      pump(5, until = function() client$dials >= 3L)
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_gte(client$dials, 3L)
  msgs = cli::ansi_strip(msgs)
  expect_equal(sum(grepl("Lost the broadcast, reconnecting: the broadcast closed the connection.", msgs, fixed = TRUE)), 1)
  expect_equal(sum(grepl("Still reconnecting: no broadcast at this address.", msgs, fixed = TRUE)), 1)
  expect_equal(sum(grepl("reconnecting", msgs)), 2)
})

test_that("a running follower of the same address and port is found, to be reused", {
  follower = suppressMessages(lc_follower$new(
    function() fake_client(list(fake_conn())), port = 4000L, address = "tcSAME"
  ))
  register_follower(follower)
  withr::defer(if (follower$is_running()) suppressMessages(follower$stop()))

  expect_identical(find_follower("tcSAME", 4000L), follower)
  expect_null(find_follower("tcOTHER", 4000L))
  expect_null(find_follower("tcSAME", 4001L))

  # A broadcast that has ended is a session of its own: follow() starts anew.
  suppressMessages(follower$receive(c(hello_bytes(), lines_bytes(tc_bye()))))
  expect_null(find_follower("tcSAME", 4000L))

  again = suppressMessages(lc_follower$new(
    function() fake_client(list(fake_conn())), port = 4000L, address = "tcSAME"
  ))
  register_follower(again)
  expect_identical(find_follower("tcSAME", 4000L), again)
  suppressMessages(again$stop())
  expect_null(find_follower("tcSAME", 4000L))
})

test_that("unfollow stops every follower, or the one whose page is on a port", {
  expect_message(expect_equal(unfollow(), 0L), "Not following")

  a = local_follower(fake_client(list(fake_conn())))
  b = local_follower(fake_client(list(fake_conn())))
  expect_false(identical(a$local_port, b$local_port))
  expect_match(a$url, paste0(":", a$local_port, "/"), fixed = TRUE)

  expect_error(unfollow("x"), "port")
  expect_error(unfollow(c(1, 2)), "port")
  free = setdiff(1024:65535, c(a$local_port, b$local_port))[1]
  expect_error(unfollow(free), "No follower")
  expect_true(a$is_running() && b$is_running())

  expect_message(expect_equal(unfollow(a$local_port), 1L), "Stopped following")
  expect_false(a$is_running())
  expect_true(b$is_running())

  suppressMessages(expect_equal(unfollow(), 1L))
  expect_false(b$is_running())
  expect_length(.globals$followers, 0)
})

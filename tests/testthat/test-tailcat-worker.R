skip_if_not_installed("promises")

test_that("the hub greets, sends to every follower, and never reads", {
  hub = tc_hub$new(max_clients = 10)
  a = fake_conn()
  b = fake_conn()
  hub$add(a, "hello\n")
  hub$add(b, "hello\n")
  pump(0.2)
  hub$send("one\n")
  pump(0.2)
  hub$send("two\n")
  pump(0.2)

  expect_equal(hub$n(), 2)
  expect_equal(unlist(a$written), c("hello\n", "one\n", "two\n"))
  expect_equal(unlist(b$written), c("hello\n", "one\n", "two\n"))
  expect_equal(a$reads + b$reads, 0L)
  expect_true(all(c(a$timeouts, b$timeouts) == tc_write_timeout))
  expect_equal(tc_write_timeout, 5)
})

test_that("a follower with a write still pending is dropped without holding up others", {
  hub = tc_hub$new()
  slow = fake_conn(write = "hang")
  fast = fake_conn()
  hub$add(slow, "hello\n")
  hub$add(fast, "hello\n")
  pump(0.2)

  hub$send("one\n")
  pump(0.2)
  expect_true(slow$closed)
  expect_length(slow$written, 1)
  expect_equal(hub$n(), 1)
  expect_equal(hub$n_dropped(), 1)

  hub$send("two\n")
  pump(0.2)
  expect_equal(unlist(fast$written), c("hello\n", "one\n", "two\n"))
  expect_false(fast$closed)
  expect_equal(slow$reads + fast$reads, 0L)
})

test_that("a follower whose write fails or errors is dropped", {
  changes = 0
  hub = tc_hub$new(on_change = function() changes <<- changes + 1)
  failing = fake_conn(write = "fail")
  closed = fake_conn(write = "throw")
  ok = fake_conn()
  hub$add(failing, "hello\n")
  hub$add(closed, "hello\n")
  hub$add(ok, "hello\n")
  pump(0.2)

  expect_true(failing$closed)
  expect_true(closed$closed)
  expect_false(ok$closed)
  expect_equal(hub$n(), 1)
  expect_equal(changes, 5)

  hub$close_all()
  expect_true(ok$closed)
  expect_equal(hub$n(), 0)
})

test_that("connections over max_clients are closed straight away", {
  hub = tc_hub$new(max_clients = 2)
  conns = replicate(3, fake_conn(), simplify = FALSE)
  for (conn in conns) hub$add(conn, "hello\n")
  pump(0.2)

  expect_equal(hub$n(), 2)
  expect_equal(hub$n_refused(), 1)
  expect_true(conns[[3]]$closed)
  expect_length(conns[[3]]$written, 0)
  expect_equal(conns[[3]]$reads, 0L)
})

test_that("the worker never issues a read on a connection", {
  calls = unlist(lapply(
    c(list(tailcat_worker_run, tc_greeting, tc_apply_frames), tc_hub$public_methods, tc_hub$private_methods),
    function(f) all.names(body(f))
  ))
  expect_false(any(calls %in% c("read", "read_async", "read_all", "read_all_async", "readable")))
  expect_true("write_async" %in% calls)
})

skip_on_os("windows")

test_that("the spool directory and its files are readable by the owner only", {
  dir = tc_spool_create()
  on.exit(unlink(dir, recursive = TRUE))
  expect_equal(file_mode(dir), "700")

  old = Sys.umask("022")
  on.exit(Sys.umask(old), add = TRUE)
  tc_status_write(dir, list(state = "ready", address = "not-a-real-address", clients = 0L))
  expect_equal(file_mode(file.path(dir, "status.json")), "600")
  expect_equal(tc_status_read(dir)$address, "not-a-real-address")

  tc_spool_write(dir, tc_spool_frame_name(1), "{}")
  expect_equal(file_mode(file.path(dir, tc_spool_frame_name(1))), "600")
  expect_length(list.files(dir, pattern = "^\\.tmp-", all.files = TRUE), 0)
})

test_that("spool frames are taken in order and deleted", {
  dir = tc_spool_create()
  on.exit(unlink(dir, recursive = TRUE))

  for (i in c(10, 2, 1, 100))
    tc_spool_write(dir, tc_spool_frame_name(i), jsonlite::toJSON(list(content = as.character(i)), auto_unbox = TRUE))
  writeLines("{\"content\": \"partial\"", file.path(dir, ".tmp-frame-000000000003.json"))

  frames = tc_spool_take(dir)
  expect_equal(vapply(frames, function(f) f$content, ""), c("1", "2", "10", "100"))
  expect_length(tc_spool_take(dir), 0)
  expect_true(file.exists(file.path(dir, ".tmp-frame-000000000003.json")))
})

test_that("a sink gets the full state, then every frame, and releases messages", {
  f = local_test_file("x = 1")
  srv = file_stream_server("127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, interval = 10)
  on.exit(srv$stop())

  got = list()
  srv$set_sticky_msg(noty_msg$new("<b>hi</b>", plain_text = "hi"), announce = FALSE)
  srv$add_sink("test", function(frame) got[[length(got) + 1]] <<- tc_spool_frame(frame))
  expect_equal(got[[1]]$content, "x = 1\n")
  expect_equal(got[[1]]$selection, "")
  expect_equal(got[[1]]$sticky$text, "hi")

  srv$add_msg("queued")
  touch_write(f, "x = 2")
  srv$emit(srv$build_frame())
  expect_equal(got[[2]]$content, "x = 2\n")
  expect_equal(got[[2]]$messages[[1]]$text, "queued")
  expect_false(srv$have_msgs())

  srv$remove_sink("test")
  srv$emit(list(content = "y"))
  expect_length(got, 2)
})

test_that("a failing sink is reported once and does not stop the loop", {
  f = local_test_file("x = 1")
  srv = file_stream_server("127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, interval = 10)
  on.exit(srv$stop())

  calls = 0
  msgs = capture_messages({
    srv$add_sink("Broken sink", function(frame) {
      calls <<- calls + 1
      stop("nope")
    })
    srv$emit(list(content = "a"))
    srv$emit(list(content = "b"))
  })
  expect_equal(calls, 3)
  expect_equal(sum(grepl("Broken sink failed", msgs)), 1)
})

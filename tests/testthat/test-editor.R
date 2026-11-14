callbacks_at_start = getTaskCallbackNames()

test_that("the backend is detected from the environment", {
  withr::local_options(livecode.editor = NULL)

  local_mocked_bindings(rs_available = function() FALSE)
  expect_equal(editor_backend(), "none")

  local_mocked_bindings(rs_available = function() stop("no rstudioapi"))
  expect_equal(editor_backend(), "none")

  local_mocked_bindings(rs_available = function() TRUE, platform_gui = function() "RStudio")
  withr::local_envvar(POSITRON = "")
  expect_equal(editor_backend(), "rstudio")

  # RStudio started from a Positron terminal inherits POSITRON=1.
  withr::local_envvar(POSITRON = "1")
  expect_equal(editor_backend(), "rstudio")

  local_mocked_bindings(platform_gui = function() "Positron")
  expect_equal(editor_backend(), "positron")

  # ark outside Positron, for example in Jupyter.
  withr::local_envvar(POSITRON = "")
  expect_equal(editor_backend(), "none")

  # R started in a Positron terminal: POSITRON=1 but no rstudioapi shims.
  withr::local_envvar(POSITRON = "1")
  local_mocked_bindings(rs_available = function() FALSE, platform_gui = function() "X11")
  expect_equal(editor_backend(), "none")
})

test_that("the livecode.editor option overrides detection", {
  local_mocked_bindings(rs_available = function() TRUE, platform_gui = function() "RStudio")

  withr::local_options(livecode.editor = "positron")
  expect_equal(editor_backend(), "positron")

  withr::local_options(livecode.editor = "none")
  expect_equal(editor_backend(), "none")

  withr::local_options(livecode.editor = "vim")
  expect_error(editor_backend(), "livecode.editor")
})

test_that("a missing editor context is NULL and never errors", {
  local_editor("rstudio")
  ed = fake_editor()
  ed$ctx = NULL
  expect_null(editor_context())

  local_mocked_bindings(rs_context = function() stop("RStudio not running"))
  msgs = capture_messages({
    expect_null(editor_context())
    expect_null(editor_context())
  })
  expect_equal(sum(grepl("RStudio not running", msgs)), 1)

  expect_null(editor_context("none"))
})

test_that("untitled documents have no path in either IDE", {
  local_editor("rstudio")
  ed = fake_editor(path = "", id = "1A2B")
  ctx = editor_context()
  expect_null(ctx$path)
  expect_equal(ctx$id, "1A2B")

  local_editor("positron")
  ed = fake_editor(path = "Untitled-1")
  ctx = editor_context()
  expect_false(is.null(ctx))
  expect_null(ctx$path)
  expect_null(ctx$id)
})

test_that("paths are compared after resolving symlinks", {
  skip_on_os("windows")
  dir = withr::local_tempdir()
  real = file.path(dir, "real")
  dir.create(real)
  writeLines("x = 1", file.path(real, "a.R"))
  file.symlink(real, file.path(dir, "link"))

  expect_true(same_path(file.path(dir, "link", "a.R"), file.path(real, "a.R")))
  expect_true(same_path(file.path(dir, "link", "new.R"), file.path(real, "new.R")))
  expect_false(same_path(file.path(real, "a.R"), file.path(real, "b.R")))
  expect_false(same_path(NULL, file.path(real, "a.R")))
})

test_that("the selection is sent only for the served file", {
  f = local_test_file()
  local_editor("positron")
  ed = fake_editor(path = f, selection = sel_rows(3, 4))

  expect_equal(editor_selection(f, editor_context()), "3-4")
  expect_equal(editor_selection(paste0(f, "x"), editor_context()), "")
  expect_equal(editor_selection(f, NULL), "")

  ed$ctx$path = "Untitled-1"
  expect_equal(editor_selection(f, editor_context()), "")

  expect_null(editor_selection(f, NULL, "none"))
})

test_that("RStudio matches the served document by id, so a renamed one stays served", {
  f = local_test_file()
  ctx = list(id = "ID1", path = "/elsewhere/renamed.R", selection = sel_rows(2, 2))
  expect_true(editor_is_served(f, "ID1", ctx))
  expect_false(editor_is_served(f, "ID2", list(id = "ID1", path = f)))
  expect_true(editor_is_served(f, NULL, list(id = NULL, path = f)))
  expect_equal(editor_selection(f, ctx, "rstudio", id = "ID1"), "2")
})

test_that("editor_save saves RStudio documents by id and reports a failing save once", {
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2")

  expect_true(editor_save("ID1"))
  expect_equal(calls_of(ed, "save"), list(list("save", "ID1")))

  expect_false(editor_save(NULL))
  expect_false(editor_save("ID1", "positron"))
  expect_false(editor_save("ID1", "none"))
  expect_length(calls_of(ed, "save"), 1)

  local_mocked_bindings(rs_save = function(id = NULL) stop("no such document"))
  msgs = capture_messages({
    expect_false(editor_save("ID1"))
    expect_false(editor_save("ID1"))
  })
  expect_equal(sum(grepl("no such document", msgs)), 1)
})

test_that("editor_open waits for the requested file", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = "/elsewhere/other.R")
  local_mocked_bindings(rs_navigate = function(file) ed$ctx$path = file)

  ctx = editor_open(f, timeout = 1, poll = 0.01)
  expect_true(same_path(ctx$path, f))
})

test_that("a relative path is matched against the editor's absolute path", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  withr::local_dir(dirname(f))

  srv = suppressMessages(serve_file(basename(f), ip = "127.0.0.1", open_browser = FALSE))
  withr::defer(suppressMessages(srv$stop()))
  expect_true(same_path(srv$path, f))
  expect_length(calls_of(ed, "navigate"), 0)
})

test_that("editor_open errors instead of serving another document", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID1")

  expect_error(editor_open(f, timeout = 0.2, poll = 0.01), "did not show")
  expect_length(calls_of(ed, "navigate"), 1)

  ed$ctx$path = f
  ed$calls = list()
  expect_true(same_path(editor_open(f, timeout = 0.2)$path, f))
  expect_length(calls_of(ed, "navigate"), 0)
})

test_that("serve_file uses the active document or explains why it cannot", {
  local_editor("positron")
  ed = fake_editor()

  ed$ctx = NULL
  expect_error(serve_file(ip = "127.0.0.1", open_browser = FALSE), "no document is open")

  ed$ctx = list(path = "Untitled-1", selection = list())
  expect_error(serve_file(ip = "127.0.0.1", open_browser = FALSE), "has not been saved")

  ed$ctx = list(path = "/elsewhere/other.R", selection = list())
  f = local_test_file()
  expect_error(
    serve_file(f, ip = "127.0.0.1", open_browser = FALSE, auto_save = FALSE),
    "did not show"
  )

  local_editor("none")
  expect_error(serve_file(ip = "127.0.0.1", open_browser = FALSE), "No file specified")
  expect_error(serve_file(character(), ip = "127.0.0.1", open_browser = FALSE), "single file path")
  expect_error(serve_file("", ip = "127.0.0.1", open_browser = FALSE), "single file path")
  expect_length(list_servers(), 0)
})

test_that("serve_file with no file serves the active document", {
  skip_if_not_installed("curl")
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")

  srv = suppressMessages(serve_file(ip = "127.0.0.1", open_browser = FALSE, auto_save = FALSE))
  withr::defer(suppressMessages(srv$stop()))
  expect_true(same_path(srv$path, f))
})

test_that("an RStudio tick broadcasts the unsaved buffer read by id while another tab is active", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2", contents = "y = 1", selection = sel_rows(1, 1))
  ed$docs$ID1 = list(id = "ID1", path = f, contents = c("x = 2 # unsaved", ""), selection = sel_rows(1, 1))

  srv = local_lc_server(f, "rstudio", file_id = "ID1")
  log = frame_log(srv)
  srv$tick()

  expect_equal(ed$id_reads, 1)
  expect_equal(srv$connect_frame()$content, "x = 2 # unsaved\n")
  expect_equal(srv$connect_frame()$selection, "")
  expect_equal(readLines(f), "x = 1")
  expect_length(calls_of(ed, "save"), 0)

  # Content is sent only when it changed since the last broadcast.
  srv$tick()
  expect_equal(log$frames[[2]]$content, "x = 2 # unsaved\n")
  expect_null(log$frames[[3]]$content)
})

test_that("the RStudio selection is broadcast only while the served document is active", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1", contents = c("x = 1", ""), selection = sel_rows(2, 3))
  srv = local_lc_server(f, "rstudio", file_id = "ID1")

  srv$tick()
  expect_equal(srv$connect_frame()$selection, "2-3")

  ed$docs$ID1 = ed$ctx
  ed$ctx = list(id = "ID2", path = "/elsewhere/other.R", contents = "y = 1", selection = sel_rows(1, 1))
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "")
  expect_equal(srv$connect_frame()$content, "x = 1\n")
})

test_that("RStudio falls back to the active document when reading by id fails, without retrying", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2", contents = "y = 1")
  ed$docs$ID1 = function() stop("unused argument (id)")
  srv = local_lc_server(f, "rstudio", file_id = "ID1")

  msgs = capture_messages(for (i in 1:3) srv$tick())
  expect_equal(sum(grepl("by its document id", msgs)), 1)
  expect_equal(ed$id_reads, 1)
  expect_equal(srv$connect_frame()$content, "x = 1\n")
  expect_equal(srv$connect_frame()$selection, "")

  ed$ctx = list(id = "ID1", path = f, contents = c("x = 3", ""), selection = sel_rows(1, 1))
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 3\n")
  expect_equal(srv$connect_frame()$selection, "1")
  expect_equal(ed$id_reads, 1)
})

test_that("an RStudio that answers a by-id read with another document is not trusted", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2", contents = "y = 1")
  ed$docs$ID1 = function() ed$ctx
  srv = local_lc_server(f, "rstudio", file_id = "ID1")

  msgs = capture_messages(srv$tick())
  expect_true(any(grepl("another document", msgs)))
  expect_equal(srv$connect_frame()$content, "x = 1\n")
})

test_that("RStudio saves only with auto_save = TRUE, by id", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2")
  ed$docs$ID1 = list(id = "ID1", path = f, contents = "x = 2")

  srv = local_lc_server(f, "rstudio", file_id = "ID1")
  for (i in 1:2) srv$tick()
  expect_length(calls_of(ed, "save"), 0)
  srv$stop()

  srv = local_lc_server(f, "rstudio", file_id = "ID1", auto_save = TRUE)
  for (i in 1:2) srv$tick()
  expect_equal(calls_of(ed, "save"), list(list("save", "ID1"), list("save", "ID1")))
})

test_that("content comes from disk when the editor gives nothing", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor()
  ed$ctx = NULL
  srv = local_lc_server(f, "rstudio", file_id = "ID1")

  suppressMessages(srv$tick())
  expect_equal(srv$connect_frame()$content, "x = 1\n")

  touch_write(f, "x = 2")
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 2\n")

  ed$ctx = list(id = "ID1", path = f, contents = NULL, selection = list())
  touch_write(f, "x = 3")
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 3\n")
})

test_that("RStudio follows the served document through a rename", {
  f = local_test_file("x = 1")
  renamed = file.path(dirname(f), "renamed.R")
  writeLines("x = 1", renamed)
  withr::defer(unlink(renamed))
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1", contents = c("x = 1", ""))
  srv = local_lc_server(f, "rstudio", file_id = "ID1")
  expect_no_message(srv$tick())

  ed$ctx$path = renamed
  ed$ctx$contents = c("x = 2", "")
  msgs = capture_messages({
    srv$tick()
    srv$tick()
  })
  expect_equal(sum(grepl("renamed or saved as", msgs)), 1)
  expect_true(same_path(srv$path(), renamed))
  expect_equal(srv$connect_frame()$content, "x = 2\n")
  expect_equal(srv$connect_frame()$selection, "")
})

test_that("a late joiner does not swallow a buffer update for existing clients", {
  skip_if_not_installed("websocket")
  skip_if_not_installed("curl")
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1", contents = c("x = 1", ""))
  port = httpuv::randomPort(host = "127.0.0.1")
  srv = local_lc_server(f, "rstudio", file_id = "ID1", interval = 0.3, port = port)
  url = paste0("http://127.0.0.1:", port)

  a = ws_client(url)
  pump(0.5)

  ed$ctx$contents = c("x = 2", "")
  b = ws_client(url)

  pump(5, until = function() "x = 2\n" %in% contents(a) && "x = 2\n" %in% contents(b))
  expect_true("x = 2\n" %in% contents(a))
  expect_true("x = 2\n" %in% contents(b))
})

test_that("a blocking Positron tick reads the active editor and holds the last buffer", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 5", ""), selection = sel_rows(1, 2))
  srv = local_lc_server(f, "positron")
  srv$set_blocking(TRUE)

  srv$tick()
  expect_equal(ed$id_reads, 0)
  expect_gt(ed$reads, 0)
  expect_equal(srv$connect_frame()$content, "x = 5\n")
  expect_equal(srv$connect_frame()$selection, "1-2")

  ed$ctx = list(path = "/elsewhere/other.R", contents = "y = 1", selection = sel_rows(1, 1))
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 5\n")
  expect_equal(srv$connect_frame()$selection, "")

  ed$ctx = NULL
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 5\n")
  expect_equal(srv$connect_frame()$selection, "")

  # A change on disk while the buffer cannot be read is newer than the
  # buffer held.
  touch_write(f, "x = 6")
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 6\n")
})

test_that("a non-blocking Positron tick makes no editor calls", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = "x = 5", selection = sel_rows(1, 2))
  srv = local_lc_server(f, "positron")

  msgs = capture_messages(for (i in 1:3) srv$tick())
  expect_length(msgs, 0)
  expect_equal(ed$reads, 0)
  expect_equal(ed$id_reads, 0)
  expect_length(ed$calls, 0)
  expect_equal(srv$connect_frame()$content, "x = 1\n")
  expect_null(srv$connect_frame()$selection)

  touch_write(f, "x = 2")
  srv$tick()
  expect_equal(srv$connect_frame()$content, "x = 2\n")
  expect_equal(ed$reads, 0)
})

test_that("serve_file with block = FALSE in Positron explains what it cannot do", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f)

  msgs = capture_messages({
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE, block = FALSE)
  })
  withr::defer(suppressMessages(srv$stop()))
  expect_true(srv$is_running())
  expect_true(any(grepl("block = FALSE", msgs, fixed = TRUE)))
  expect_true(any(grepl("files.autoSave", msgs, fixed = TRUE)))
})

test_that("auto_save = TRUE is an error in Positron", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f)

  expect_error(
    serve_file(f, ip = "127.0.0.1", open_browser = FALSE, auto_save = TRUE, block = FALSE),
    "not supported in Positron"
  )
  expect_length(ed$calls, 0)
  expect_length(list_servers(), 0)
  expect_error(check_auto_save(NA, "rstudio"), "auto_save")
})

test_that("block defaults to TRUE in Positron only", {
  expect_true(resolve_block(NULL, "positron"))
  expect_false(resolve_block(NULL, "rstudio"))
  expect_false(resolve_block(NULL, "none"))
  expect_true(resolve_block(TRUE, "rstudio"))
  expect_false(resolve_block(FALSE, "positron"))
  expect_error(resolve_block("yes", "none"), "block")

  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  srv = suppressMessages(serve_file(ip = "127.0.0.1", open_browser = FALSE))
  withr::defer(suppressMessages(srv$stop()))
  expect_true(srv$is_running())
})

test_that("a blocking serve_file returns when the server is stopped from a callback", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  local_editor("none")

  later::later(function() stop_all(), 0.5)
  start = Sys.time()
  msgs = capture_messages(with_time_limit(10, {
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE, block = TRUE)
  }))
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 10)
  expect_false(any(grepl("after an error", msgs)))
  expect_true(any(grepl("Stopped server", msgs)))
  expect_s3_class(srv, "LiveCodeServer_Interface")
  expect_false(srv$is_running())
  expect_length(list_servers(), 0)
  expect_equal(getTaskCallbackNames(), before)
})

test_that("an interrupt ends a blocking serve_file cleanly", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  local_editor("none")
  steps = 0
  local_mocked_bindings(block_step = function(timeout = 0.1) {
    steps <<- steps + 1
    if (steps >= 3)
      signalCondition(interrupt_condition())
    later::run_now(0.02)
  })

  msgs = capture_messages(with_time_limit(10, {
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE, block = TRUE)
  }))
  expect_equal(steps, 3)
  expect_equal(sum(grepl("Stopped sharing", msgs)), 1)
  expect_false(any(grepl("Stopped server", msgs)))
  expect_false(srv$is_running())
  expect_length(list_servers(), 0)
  expect_equal(getTaskCallbackNames(), before)
})

test_that("an error in the blocking loop stops the server and is reported", {
  f = local_test_file("x = 1")
  local_editor("none")
  local_mocked_bindings(block_step = function(timeout = 0.1) stop("loop broke"))

  msgs = capture_messages(with_time_limit(10, {
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE, block = TRUE)
  }))
  expect_equal(sum(grepl("after an error: loop broke", msgs)), 1)
  expect_false(srv$is_running())
  expect_length(list_servers(), 0)
})

test_that("a blocking Positron serve_file reads the editor from inside the loop", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 7", ""), selection = sel_rows(1, 1))
  seen = NULL
  later::later(function() {
    seen <<- list_servers()[[1]]$.__enclos_env__$private$server$connect_frame()
    stop_all()
  }, 0.5)

  msgs = capture_messages(with_time_limit(10, {
    srv = serve_file(f, ip = "127.0.0.1", open_browser = FALSE)
  }))
  expect_true(any(grepl("second R console", msgs)))
  expect_equal(seen$content, "x = 7\n")
  expect_equal(seen$selection, "1")
  expect_false(srv$is_running())
  expect_equal(getTaskCallbackNames(), before)
})

test_that("RStudio warns about stripping whitespace from the global or project setting", {
  local_editor("rstudio")
  proj = withr::local_tempdir()
  writeLines(c("Version: 1.0", "StripTrailingWhitespace: Yes"), file.path(proj, "p.Rproj"))

  local_mocked_bindings(rs_active_project = function() proj, rs_read_pref = function(name, default) default)
  expect_message(editor_notes(auto_save = TRUE), "Project Options")

  local_mocked_bindings(rs_active_project = function() NULL, rs_read_pref = function(name, default) TRUE)
  expect_message(editor_notes(auto_save = TRUE), "Global Options")

  local_mocked_bindings(rs_read_pref = function(name, default) default)
  expect_no_message(editor_notes(auto_save = TRUE))
})

local_page_openers = function(viewer = function(url) NULL, env = parent.frame()) {
  opened = new.env()
  opened$viewer = character()
  opened$browser = character()
  testthat::local_mocked_bindings(
    rs_viewer = function(url) {
      opened$viewer = c(opened$viewer, url)
      viewer(url)
    },
    browse_url = function(url) opened$browser = c(opened$browser, url),
    .env = env
  )
  opened
}

test_that("Positron shows the page in the Viewer pane, other backends in the browser", {
  local_mocked_bindings(positron_mode = function() "desktop")
  opened = local_page_openers()
  expect_equal(editor_show_page("http://192.168.7.46:42244", "positron"), "viewer")
  expect_equal(opened$viewer, "http://192.168.7.46:42244")
  expect_length(opened$browser, 0)

  expect_equal(editor_show_page("http://192.168.7.46:42244", "rstudio"), "browser")
  expect_equal(editor_show_page("http://192.168.7.46:42244", "none"), "browser")
  expect_length(opened$viewer, 1)
  expect_length(opened$browser, 2)
})

test_that("a failing viewer falls back to the browser", {
  opened = local_page_openers(viewer = function(url) stop("No active request"))
  expect_equal(editor_show_page("http://192.168.7.46:42244", "positron"), "browser")
  expect_length(opened$viewer, 1)
  expect_equal(opened$browser, "http://192.168.7.46:42244")
})

test_that("a local page goes through browseURL, which Positron shows in its Viewer", {
  local_mocked_bindings(positron_mode = function() "desktop")
  opened = local_page_openers()
  expect_equal(editor_show_page("http://127.0.0.1:5000", "positron", local = TRUE), "browser")
  expect_length(opened$viewer, 0)
  expect_equal(opened$browser, "http://127.0.0.1:5000")
})

test_that("serve_file opens the Viewer before returning in Positron", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f)
  opened = local_page_openers()

  srv = suppressMessages(serve_file(f, ip = "127.0.0.1", block = FALSE))
  withr::defer(suppressMessages(srv$stop()))
  expect_equal(opened$viewer, srv$url)
  expect_length(opened$browser, 0)
})

test_that("serve_file opens the page later, from the event loop, in RStudio", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  opened = local_page_openers()

  srv = suppressMessages(serve_file(f, ip = "127.0.0.1"))
  withr::defer(suppressMessages(srv$stop()))
  expect_length(opened$viewer, 0)
  pump(3, until = function() length(opened$viewer) > 0)
  expect_equal(opened$viewer, srv$url)
  expect_length(opened$browser, 0)
})

test_that("in an IDE a server on a network address shows its Viewer a local copy of the page", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  opened = local_page_openers()
  # Stands in for a network address, which a test machine may not have.
  local_mocked_bindings(is_ip_loopback = function(ip) FALSE)

  srv = suppressMessages(serve_file(f, ip = "127.0.0.1", interval = 0.1))
  withr::defer(if (srv$is_running()) suppressMessages(srv$stop()))
  pump(3, until = function() length(opened$viewer) > 0)
  local = opened$viewer
  expect_length(local, 1)
  expect_length(opened$browser, 0)
  expect_match(local, "^http://127\\.0\\.0\\.1:[0-9]+/[0-9a-f]{32}/$")
  expect_false(startsWith(local, srv$url))

  base = sub("^(http://[^/]+)/.*$", "\\1", local)
  expect_equal(http_get(local)$status_code, 200L)
  expect_match(rawToChar(http_get(local)$content), "id=\"livecode-view\"", fixed = TRUE)
  expect_equal(http_get(paste0(local, "web/livecode/livecode.js"))$status_code, 200L)
  expect_equal(http_get(paste0(base, "/"))$status_code, 403L)
  expect_equal(http_get(paste0(base, "/", strrep("0", 32), "/"))$status_code, 403L)
  expect_equal(http_get(paste0(base, "/web/livecode/livecode.js"))$status_code, 403L)

  # The local page gets what the students' page gets.
  page = ws_client(local)
  expect_equal(page$frames[[1]]$content, "x = 1\n")
  touch_write(f, "x = 2")
  ed$ctx$contents = "x = 2"
  pump(3, until = function() any(startsWith(contents(page), "x = 2")))
  expect_true(any(startsWith(contents(page), "x = 2")))

  outsider = ws_client(base)
  expect_length(outsider$frames, 0)

  # The address survives a restart, so an open Viewer finds the server again.
  suppressMessages(srv$restart())
  expect_equal(http_get(local)$status_code, 200L)
  srv$open()
  expect_equal(opened$viewer, c(local, local))

  suppressMessages(srv$stop())
  expect_equal(http_get(local, seconds = 2)$status_code, NA)
})

test_that("the local copy is not used outside an IDE, without the Viewer, or on 127.0.0.1", {
  f = local_test_file("x = 1")
  opened = local_page_openers()

  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  srv = suppressMessages(serve_file(f, ip = "127.0.0.1", open_browser = FALSE))
  srv$open()
  expect_equal(opened$viewer, srv$url)
  suppressMessages(srv$stop())

  local_mocked_bindings(is_ip_loopback = function(ip) FALSE)
  withr::with_options(list(livecode.viewer = FALSE), {
    srv = suppressMessages(serve_file(f, ip = "127.0.0.1", open_browser = FALSE))
    srv$open()
    expect_equal(opened$browser, srv$url)
    suppressMessages(srv$stop())
  })

  local_editor("none")
  srv = suppressMessages(serve_file(f, ip = "127.0.0.1", open_browser = FALSE))
  srv$open()
  expect_equal(opened$browser[2], srv$url)
  suppressMessages(srv$stop())
  expect_length(opened$viewer, 1)
})

test_that("the tests leave no task callback registered", {
  expect_equal(getTaskCallbackNames(), callbacks_at_start)
})

test_that("a local page in a Positron Workbench session is shown in the Viewer", {
  local_mocked_bindings(positron_mode = function() "server")
  opened = local_page_openers()
  expect_equal(editor_show_page("http://127.0.0.1:5000", "positron", local = TRUE), "viewer")
  expect_equal(opened$viewer, "http://127.0.0.1:5000")
  expect_length(opened$browser, 0)
})

test_that("RStudio shows pages of this computer in the Viewer, others in the browser", {
  opened = local_page_openers()
  expect_equal(editor_show_page("http://127.0.0.1:5000", "rstudio", local = TRUE), "viewer")
  expect_equal(editor_show_page("http://localhost:5000/", "rstudio"), "viewer")
  expect_equal(editor_show_page("http://192.168.7.46:42244", "rstudio"), "browser")
  expect_equal(editor_show_page("http://127.0.0.1.evil.example:5000", "rstudio"), "browser")
  expect_length(opened$viewer, 2)
  expect_length(opened$browser, 2)
})

test_that("options(livecode.viewer = FALSE) always uses the browser", {
  withr::local_options(livecode.viewer = FALSE)
  local_mocked_bindings(positron_mode = function() "server")
  opened = local_page_openers()
  expect_equal(editor_show_page("http://192.168.7.46:42244", "positron"), "browser")
  expect_equal(editor_show_page("http://127.0.0.1:5000", "positron", local = TRUE), "browser")
  expect_equal(editor_show_page("http://127.0.0.1:5000", "rstudio", local = TRUE), "browser")
  expect_length(opened$viewer, 0)
  expect_length(opened$browser, 3)
})

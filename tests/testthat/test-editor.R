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

test_that("Positron does not save when another editor is active", {
  f = local_test_file("x = 1")
  other = local_test_file("y = 2")
  local_editor("positron")
  ed = fake_editor(path = other, contents = "y = 3")

  expect_false(editor_save(f, NULL, editor_context()))
  expect_length(ed$calls, 0)

  ed$ctx = NULL
  expect_false(editor_save(f, NULL, editor_context()))
  expect_length(ed$calls, 0)
})

test_that("Positron saves the served file without formatting and without an id", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 2", ""))

  expect_true(editor_save(f, NULL, editor_context()))
  expect_equal(calls_of(ed, "command"), list(list("command", "workbench.action.files.saveWithoutFormatting")))
  expect_length(calls_of(ed, "save"), 0)
})

test_that("Positron skips the save when the buffer matches the file", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 1", ""))

  expect_false(editor_save(f, NULL, editor_context()))
  expect_length(ed$calls, 0)
})

test_that("Positron falls back to documentSave() once save without formatting fails", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = "x = 2")
  tries = 0
  local_mocked_bindings(ps_execute_command = function(command) {
    tries <<- tries + 1
    stop("not available")
  })

  msgs = capture_messages({
    expect_true(editor_save(f, NULL, editor_context()))
    expect_true(editor_save(f, NULL, editor_context()))
  })
  expect_equal(tries, 1)
  expect_equal(calls_of(ed, "save"), list(list("save", NULL), list("save", NULL)))
  expect_equal(sum(grepl("formatOnSave", msgs)), 1)
})

test_that("RStudio saves by id and reports a failing save once", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID1")

  expect_true(editor_save(f, "ID1", editor_context()))
  expect_equal(calls_of(ed, "save"), list(list("save", "ID1")))

  expect_false(editor_save(f, NULL, editor_context()))
  expect_length(calls_of(ed, "save"), 1)

  local_mocked_bindings(rs_save = function(id = NULL) stop("no such document"))
  msgs = capture_messages({
    expect_false(editor_save(f, "ID1", NULL))
    expect_false(editor_save(f, "ID1", NULL))
  })
  expect_equal(sum(grepl("no such document", msgs)), 1)

  expect_false(editor_save(f, "ID1", NULL, "none"))
})

test_that("editor_open waits for the requested file", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = "/elsewhere/other.R")
  local_mocked_bindings(rs_navigate = function(file) ed$ctx$path = file)

  ctx = editor_open(f, timeout = 1, poll = 0.01)
  expect_true(same_path(ctx$path, f))
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

test_that("a Positron tick with another editor active saves nothing and clears the highlight", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = "/elsewhere/other.R", contents = "y = 2", selection = sel_rows(1, 2))

  srv = file_stream_server(
    "127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f,
    interval = 10, auto_save = TRUE, editor = "positron"
  )
  withr::defer(srv$stop())

  srv$tick()
  expect_length(ed$calls, 0)
  expect_equal(srv$connect_frame()$selection, "")

  ed$ctx$path = f
  ed$ctx$contents = "x = 5"
  srv$tick()
  expect_length(calls_of(ed, "command"), 1)
  expect_equal(srv$connect_frame()$selection, "1-2")
})

test_that("RStudio warns about stripping whitespace from the global or project setting", {
  local_editor("rstudio")
  proj = withr::local_tempdir()
  writeLines(c("Version: 1.0", "StripTrailingWhitespace: Yes"), file.path(proj, "p.Rproj"))

  local_mocked_bindings(rs_active_project = function() proj, rs_read_pref = function(name, default) default)
  expect_message(editor_save_warnings(), "Project Options")

  local_mocked_bindings(rs_active_project = function() NULL, rs_read_pref = function(name, default) TRUE)
  expect_message(editor_save_warnings(), "Global Options")

  local_mocked_bindings(rs_read_pref = function(name, default) default)
  expect_no_message(editor_save_warnings())
})

test_that("Positron explains its save limits once", {
  local_editor("positron")
  local_mocked_bindings(ps_command_fun = function() NULL)
  msgs = capture_messages(editor_save_warnings())
  expect_true(any(grepl("active tab", msgs)))
  expect_true(any(grepl("trimTrailingWhitespace", msgs)))

  local_editor("positron")
  local_mocked_bindings(ps_command_fun = function() function(command) NULL)
  msgs = capture_messages(editor_save_warnings())
  expect_true(any(grepl("active tab", msgs)))
  expect_false(any(grepl("trimTrailingWhitespace", msgs)))
})

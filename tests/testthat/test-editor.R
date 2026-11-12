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

test_that("a Positron tick never calls the editor", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = "x = 5", selection = sel_rows(1, 2))

  srv = local_lc_server(f, "positron")
  ed$calls = list()
  ed$reads = 0

  msgs = capture_messages(for (i in 1:3) srv$tick())
  expect_length(msgs, 0)
  expect_equal(ed$reads, 0)
  expect_length(ed$calls, 0)
})

test_that("a Positron server reads the editor at startup", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 1", ""), selection = sel_rows(2, 3))

  srv = local_lc_server(f, "positron")
  expect_equal(ed$reads, 1)
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "2-3")
})

test_that("one task callback serves every Positron server and goes with the last", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  g = local_test_file("y = 1")
  local_editor("positron")
  ed = fake_editor(path = f)

  srv1 = local_lc_server(f, "positron")
  srv2 = local_lc_server(g, "positron")
  expect_equal(sum(getTaskCallbackNames() == positron_callback_name), 1)

  srv1$stop()
  expect_true(has_positron_callback())
  srv2$stop()
  expect_false(has_positron_callback())
  expect_equal(getTaskCallbackNames(), before)
})

test_that("stop_all() removes the task callback", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f)

  srv = local_lc_server(f, "positron")
  expect_true(has_positron_callback())
  stop_all()
  expect_false(has_positron_callback())
  expect_equal(getTaskCallbackNames(), before)
})

test_that("RStudio servers register no task callback", {
  before = getTaskCallbackNames()
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")

  srv = local_lc_server(f, "rstudio", file_id = "ID1")
  expect_false(has_positron_callback())
  expect_equal(getTaskCallbackNames(), before)
})

test_that("the task callback keeps the selection of the served file only", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 1", ""), selection = sel_rows(1, 1))
  srv = local_lc_server(f, "positron")

  ed$ctx$selection = sel_rows(3, 4)
  expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "3-4")

  ed$ctx$path = "/elsewhere/other.R"
  ed$ctx$selection = sel_rows(1, 2)
  expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "")

  ed$ctx$path = f
  ed$ctx$selection = sel_rows(5, 5)
  positron_task_callback(quote(x), NULL, TRUE, FALSE)
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "5")

  ed$ctx = NULL
  positron_task_callback(quote(x), NULL, TRUE, FALSE)
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "")
})

test_that("the task callback saves only an active, changed file with auto-save on", {
  f = local_test_file("x = 1")
  other = local_test_file("y = 1")
  local_editor("positron")
  ed = fake_editor(path = f, contents = c("x = 1", ""))
  saving = local_lc_server(f, "positron", auto_save = TRUE)
  not_saving = local_lc_server(other, "positron", auto_save = FALSE)

  ed$calls = list()
  positron_task_callback(quote(x), NULL, TRUE, FALSE)
  expect_length(ed$calls, 0)

  ed$ctx$contents = "x = 2"
  positron_task_callback(quote(x), NULL, TRUE, FALSE)
  expect_equal(calls_of(ed, "command"), list(list("command", "workbench.action.files.saveWithoutFormatting")))

  ed$calls = list()
  ed$ctx$path = other
  ed$ctx$contents = "y = 2"
  positron_task_callback(quote(x), NULL, TRUE, FALSE)
  expect_length(ed$calls, 0)
})

test_that("a failing task callback still returns TRUE and reports once", {
  f = local_test_file("x = 1")
  local_editor("positron")
  ed = fake_editor(path = f)
  srv = local_lc_server(f, "positron")

  local_mocked_bindings(rs_context = function() stop("No active request"))
  msgs = capture_messages({
    expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
    expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
  })
  expect_equal(sum(grepl("No active request", msgs)), 1)

  local_mocked_bindings(positron_refresh = function() stop("unexpected"))
  msgs = capture_messages({
    expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
    expect_true(positron_task_callback(quote(x), NULL, TRUE, FALSE))
  })
  expect_equal(sum(grepl("unexpected", msgs)), 1)
})

test_that("an RStudio tick reads the editor and saves as before", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1", selection = sel_rows(2, 2))
  local_mocked_bindings(rs_document_path = function(id) f)

  srv = local_lc_server(f, "rstudio", file_id = "ID1")
  expect_equal(ed$reads, 0)
  srv$tick()
  expect_equal(ed$reads, 1)
  expect_equal(calls_of(ed, "save"), list(list("save", "ID1")))
  expect_equal(srv$connect_frame()$selection, "2")

  ed$ctx$path = "/elsewhere/other.R"
  srv$tick()
  expect_equal(srv$connect_frame()$selection, "")
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

test_that("Positron explains when the page updates", {
  local_editor("positron")
  local_mocked_bindings(ps_command_fun = function() NULL)
  msgs = capture_messages(editor_save_warnings())
  expect_true(any(grepl("each time code is run", msgs)))
  expect_true(any(grepl("files.autoSave", msgs)))
  expect_true(any(grepl("cannot save without formatting", msgs)))

  local_editor("positron")
  local_mocked_bindings(ps_command_fun = function() function(command) NULL)
  msgs = capture_messages(editor_save_warnings())
  expect_true(any(grepl("saves the file", msgs)))
  expect_false(any(grepl("cannot save without formatting", msgs)))

  local_editor("positron")
  local_mocked_bindings(ps_command_fun = function() NULL)
  msgs = capture_messages(editor_save_warnings(auto_save = FALSE))
  expect_true(any(grepl("files.autoSave", msgs)))
  expect_false(any(grepl("saves the file", msgs)))
  expect_false(any(grepl("cannot save without formatting", msgs)))
})

test_that("RStudio notices a renamed document once and stops saving it", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = f, id = "ID1")
  doc_path = f
  local_mocked_bindings(rs_document_path = function(id) doc_path)

  srv = file_stream_server(
    "127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, file_id = "ID1",
    interval = 10, auto_save = TRUE, editor = "rstudio"
  )
  withr::defer(srv$stop())

  expect_no_message(srv$tick())
  expect_length(calls_of(ed, "save"), 1)

  doc_path = file.path(dirname(f), "renamed.R")
  ed$ctx$path = doc_path
  msgs = capture_messages({
    srv$tick()
    srv$tick()
  })
  expect_equal(sum(grepl("renamed or saved as", msgs)), 1)
  expect_length(calls_of(ed, "save"), 1)
  expect_equal(srv$connect_frame()$selection, "")
})

test_that("an unknown document id is not taken for a rename", {
  f = local_test_file("x = 1")
  local_editor("rstudio")
  ed = fake_editor(path = "/elsewhere/other.R", id = "ID2")
  local_mocked_bindings(rs_document_path = function(id) stop("no document with that id"))

  srv = file_stream_server(
    "127.0.0.1", httpuv::randomPort(host = "127.0.0.1"), f, file_id = "ID1",
    interval = 10, auto_save = TRUE, editor = "rstudio"
  )
  withr::defer(srv$stop())

  expect_no_message(srv$tick())
  expect_equal(calls_of(ed, "save"), list(list("save", "ID1")))
  expect_null(editor_document_path("ID1", "positron"))
})

test_that("the tests leave no task callback registered", {
  expect_equal(getTaskCallbackNames(), callbacks_at_start)
})

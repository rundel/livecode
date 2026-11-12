# Every call into the IDE goes through this file. RStudio and Positron both
# answer rstudioapi calls (Positron through the `.rs.api.*` shims in its ark
# kernel), but they differ in what they support, so the differences live
# here and the server only sees the editor_*() functions.

# Thin wrappers around the IDE, kept separate so tests can mock them.
rs_available = function() rstudioapi::isAvailable()
rs_context = function(id = NULL) rstudioapi::getSourceEditorContext(id)
rs_save = function(id = NULL) rstudioapi::documentSave(id)
rs_navigate = function(file) rstudioapi::navigateToFile(file)
rs_read_pref = function(name, default) rstudioapi::readRStudioPreference(name, default)
rs_active_project = function() rstudioapi::getActiveProject()
rs_viewer = function(url) rstudioapi::viewer(url)
browse_url = function(url) utils::browseURL(url, browser = get_browser())
platform_gui = function() .Platform$GUI
positron_env = function() Sys.getenv("POSITRON")

editor_state = new.env(parent = emptyenv())

editor_reset = function() {
  editor_state$errors = list()
  invisible()
}

editor_reset()

editor_backends = c("rstudio", "positron", "none")

editor_backend = function() {
  opt = getOption("livecode.editor")
  if (!is.null(opt)) {
    if (!is.character(opt) || length(opt) != 1 || !opt %in% editor_backends) {
      cli::cli_abort(
        "Option {.code livecode.editor} must be one of {.or {.val {editor_backends}}}."
      )
    }
    return(opt)
  }

  available = tryCatch(isTRUE(rs_available()), error = function(e) FALSE)
  if (!available)
    return("none")

  # Positron's ark makes rstudioapi::isAvailable() return TRUE, and its
  # terminals export POSITRON=1 to any R started there, so neither alone
  # identifies the IDE.
  if (identical(platform_gui(), "RStudio"))
    "rstudio"
  else if (identical(positron_env(), "1"))
    "positron"
  else
    "none"
}

# Positron answers editor requests only while a console command runs, so by
# default serve_file() keeps the console busy there (see block_loop()).
editor_block_default = function(backend = editor_backend()) {
  identical(backend, "positron")
}

resolve_block = function(block, backend = editor_backend(), call = parent.frame()) {
  if (is.null(block))
    return(editor_block_default(backend))
  if (!is.logical(block) || length(block) != 1 || is.na(block))
    cli::cli_abort("{.arg block} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.", call = call)
  block
}

check_auto_save = function(auto_save, backend = editor_backend(), call = parent.frame()) {
  if (!is.logical(auto_save) || length(auto_save) != 1 || is.na(auto_save))
    cli::cli_abort("{.arg auto_save} must be {.code TRUE} or {.code FALSE}.", call = call)

  if (auto_save && identical(backend, "positron")) {
    cli::cli_abort(c(
      "{.code auto_save = TRUE} is not supported in Positron.",
      "i" = "livecode broadcasts the editor's unsaved text, so the file does not need saving.",
      "i" = "To save as you type, turn on Positron's own auto save ({.field files.autoSave})."
    ), call = call)
  }
  auto_save
}

# Runs an editor call, reporting a failure once until the call succeeds
# again, and never lets the error escape.
editor_try = function(label, f, default = NULL) {
  tryCatch(
    {
      res = f()
      editor_state$errors[[label]] = NULL
      res
    },
    error = function(e) {
      msg = conditionMessage(e)
      if (!identical(editor_state$errors[[label]], msg)) {
        editor_state$errors[[label]] = msg
        cli::cli_bullets(c("x" = "livecode: {label} failed, will keep retrying: {msg}"))
      }
      default
    }
  )
}

# Untitled documents have an empty path in RStudio; Positron reports the
# document's file name, which for an untitled document is not absolute.
is_saved_path = function(path) {
  is.character(path) && length(path) == 1 && !is.na(path) && nzchar(path) &&
    grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

normalize_path = function(path) {
  path = path.expand(path)
  if (file.exists(path)) {
    path = normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    dir = normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
    path = file.path(dir, basename(path))
  }

  if (.Platform$OS.type == "windows")
    path = tolower(path)

  path
}

same_path = function(a, b) {
  is_saved_path(a) && is_saved_path(b) && identical(normalize_path(a), normalize_path(b))
}

as_editor_context = function(ctx) {
  if (!is.list(ctx))
    return(NULL)

  path = ctx[["path"]]
  if (!is_saved_path(path))
    path = NULL

  id = ctx[["id"]]
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id))
    id = NULL

  list(
    id = id,
    path = path,
    selection = ctx[["selection"]],
    contents = ctx[["contents"]]
  )
}

# The active document. Returns NULL when there is no IDE or no open source
# editor, otherwise a list whose path is NULL for an untitled document.
# Positron gives no id.
editor_context = function(backend = editor_backend()) {
  if (identical(backend, "none"))
    return(NULL)

  as_editor_context(editor_try("Reading the editor", rs_context))
}

# One RStudio document by id, whether or not it is the active one. Returns
# list(ctx, error): error explains a NULL ctx.
editor_document = function(id, backend = editor_backend()) {
  if (!identical(backend, "rstudio") || is.null(id))
    return(list(ctx = NULL, error = "no document id"))

  res = tryCatch(
    list(ctx = as_editor_context(rs_context(id)), error = NULL),
    error = function(e) list(ctx = NULL, error = conditionMessage(e))
  )
  ctx = res[["ctx"]]
  if (is.null(res[["error"]]) && is.null(ctx))
    res[["error"]] = "the editor returned no document"
  # An RStudio without the id argument answers with the active document,
  # which must not be taken for the served one.
  if (!is.null(ctx) && !identical(ctx[["id"]], id))
    res = list(ctx = NULL, error = "the editor answered with another document")
  res
}

# Whether ctx, the active document, is the served one: by id when both are
# known, as RStudio keeps the id through a rename, else by path.
editor_is_served = function(file, id, ctx) {
  if (is.null(ctx))
    return(FALSE)
  if (!is.null(id) && !is.null(ctx[["id"]]))
    return(identical(ctx[["id"]], id))
  same_path(ctx[["path"]], file)
}

editor_is_active = function(file, ctx) {
  !is.null(ctx) && same_path(ctx[["path"]], file)
}

# NULL means the selection is unknown (no IDE), "" that the served file is
# not the active document, so clients clear the highlight.
editor_selection = function(file, ctx, backend = editor_backend(), id = NULL) {
  if (identical(backend, "none"))
    return(NULL)

  if (!editor_is_served(file, id, ctx))
    return("")

  extract_line_nums(ctx[["selection"]])
}

editor_text = function(contents) {
  if (!is.character(contents) || anyNA(contents))
    return(NULL)
  paste(enc2utf8(contents), collapse = "\n")
}

# Reads the served document's buffer and the selection. RStudio reads the
# document by id, so it is read while another tab is active; otherwise, and
# in Positron, the buffer is read only while the document is active.
# Returns NULL without an IDE, else list(contents, selection, path,
# by_id_error), where contents is NULL when the buffer could not be read.
editor_read = function(file, id = NULL, backend = editor_backend(), by_id = FALSE) {
  if (identical(backend, "none"))
    return(NULL)

  doc = NULL
  by_id_error = NULL
  if (by_id && identical(backend, "rstudio")) {
    res = editor_document(id, backend)
    doc = res[["ctx"]]
    by_id_error = res[["error"]]
  }

  active = editor_context(backend)
  if (is.null(doc) && editor_is_served(file, id, active))
    doc = active

  list(
    contents = editor_text(doc[["contents"]]),
    selection = editor_selection(file, active, backend, id),
    path = doc[["path"]],
    by_id_error = by_id_error
  )
}

# Opens a livecode page. Positron shows the instructor's page in its Viewer
# pane: rstudioapi::viewer() hands an http URL to a localhost proxy, which
# reaches the LAN address the server listens on. A local page (follow())
# checks the Origin of its websocket, which that proxy does not rewrite, so
# it goes through browseURL(), whose browser option Positron sets to show
# localhost URLs in the Viewer directly. Both are editor requests, so this
# must run while a console command does, not from an idle callback.
editor_show_page = function(url, backend = editor_backend(), local = FALSE) {
  if (identical(backend, "positron") && !local) {
    shown = tryCatch(
      {
        rs_viewer(url)
        TRUE
      },
      error = function(e) FALSE
    )
    if (shown)
      return(invisible("viewer"))
  }

  browse_url(url)
  invisible("browser")
}

# Opens file in the editor and waits until it is the active document, so
# the context that comes back belongs to file and not to whatever was open.
editor_open = function(file, backend = editor_backend(), timeout = 3, poll = 0.1) {
  ctx = editor_context(backend)
  if (editor_is_active(file, ctx))
    return(ctx)

  tryCatch(
    rs_navigate(file),
    error = function(e) {
      cli::cli_abort("Could not open {.file {file}} in the editor.", parent = e)
    }
  )

  deadline = Sys.time() + timeout
  repeat {
    ctx = editor_context(backend)
    if (editor_is_active(file, ctx))
      return(ctx)
    if (Sys.time() >= deadline)
      break
    Sys.sleep(poll)
  }

  cli::cli_abort(c(
    "The editor did not show {.file {file}} within {timeout} second{?s}.",
    "i" = "Open the file in the editor and try again, or set {.code options(livecode.editor = \"none\")} to serve it without the editor."
  ))
}

# Saves the served RStudio document by id. Returns TRUE if a save was
# requested, FALSE if it was skipped or failed; never errors.
editor_save = function(id, backend = editor_backend()) {
  if (!identical(backend, "rstudio") || is.null(id))
    return(FALSE)

  isTRUE(editor_try("Auto-save", function() {
    rs_save(id)
    TRUE
  }, default = FALSE))
}

# Startup notes about how the editor is followed.
editor_notes = function(backend = editor_backend(), auto_save = FALSE, block = FALSE) {
  if (identical(backend, "rstudio") && auto_save)
    rstudio_whitespace_warning()
  if (identical(backend, "positron") && !block)
    positron_nonblocking_note()
  invisible()
}

positron_blocking_note = function() {
  cli::cli_bullets(c("i" = paste(
    "This console now runs the broadcast until you interrupt it (Esc or Ctrl-C).",
    "Do your work in a second R console session."
  )))
  invisible()
}

positron_nonblocking_note = function() {
  cli::cli_bullets(c("i" = paste(
    "With {.code block = FALSE} livecode cannot read Positron's editor: the page shows the file",
    "as saved on disk and does not highlight the selection. Use the default {.code block = TRUE},",
    "or turn on Positron's auto save ({.field files.autoSave}) so the file on disk follows your typing."
  )))
  invisible()
}

rstudio_whitespace_warning = function() {
  project = tryCatch(rs_active_project(), error = function(e) NULL)
  in_project = rproj_strips_whitespace(project)
  global = isTRUE(tryCatch(
    rs_read_pref("strip_trailing_whitespace", FALSE),
    error = function(e) FALSE
  ))

  if (!in_project && !global)
    return(invisible(FALSE))

  opt_name = "Strip trailing horizontal whitespace when saving"
  menu = if (in_project)
    "Tools > Project Options > Code Editing"
  else
    "Tools > Global Options > Code > Saving"

  cli::cli_bullets(c("!" = paste(
    "You are running livecode with {.code auto_save = TRUE} and the {.val {opt_name}}",
    "option checked in RStudio. This can result in undesirable behavior while you broadcast.",
    "To resolve this, from RStudio's menu select {.field {menu}} and uncheck {.val {opt_name}}."
  )))
  invisible(TRUE)
}

rproj_strips_whitespace = function(project) {
  if (!is.character(project) || length(project) != 1 || is.na(project) || !dir.exists(project))
    return(FALSE)

  rproj = list.files(project, pattern = "\\.Rproj$", full.names = TRUE)
  if (length(rproj) == 0)
    return(FALSE)

  lines = tryCatch(readLines(rproj[[1]], warn = FALSE), error = function(e) character())
  any(grepl("^StripTrailingWhitespace:\\s*Yes\\s*$", lines))
}

# One step of the blocking loop, kept separate so tests can mock it.
block_step = function(timeout = 0.1) {
  later::run_now(timeoutSecs = timeout)
}

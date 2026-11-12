# Every call into the IDE goes through this file. RStudio and Positron both
# answer rstudioapi calls (Positron through the `.rs.api.*` shims in its ark
# kernel), but they differ in what they support, so the differences live
# here and the server only sees the editor_*() functions.

# Thin wrappers around the IDE, kept separate so tests can mock them.
rs_available = function() rstudioapi::isAvailable()
rs_context = function() rstudioapi::getSourceEditorContext()
rs_save = function(id = NULL) rstudioapi::documentSave(id)
rs_navigate = function(file) rstudioapi::navigateToFile(file)
rs_document_path = function(id) rstudioapi::documentPath(id)
rs_read_pref = function(name, default) rstudioapi::readRStudioPreference(name, default)
rs_active_project = function() rstudioapi::getActiveProject()
platform_gui = function() .Platform$GUI
positron_env = function() Sys.getenv("POSITRON")

# rstudioapi::executeCommand() cannot reach this: Positron's shim only maps a
# fixed list of RStudio command names and reports any other as missing. The
# ark function the shim forwards to is used directly instead, when present.
ps_command_fun = function() {
  env = tryCatch(as.environment("tools:positron"), error = function(e) NULL)
  if (is.null(env))
    return(NULL)
  get0(".ps.ui.executeCommand", envir = env, mode = "function", inherits = FALSE)
}

ps_execute_command = function(command) {
  f = ps_command_fun()
  if (is.null(f))
    cli::cli_abort("Positron command execution is not available.")
  f(command)
}

editor_state = new.env(parent = emptyenv())

editor_reset = function() {
  editor_state$errors = list()
  editor_state$save_unformatted = NA
  editor_state$format_note = FALSE
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
        cli::cli_alert_danger("livecode: {label} failed, will keep retrying: {msg}")
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

# Returns NULL when there is no IDE or no open source editor, otherwise a
# list whose path is NULL for an untitled document. Positron gives no id.
editor_context = function(backend = editor_backend()) {
  if (identical(backend, "none"))
    return(NULL)

  ctx = editor_try("Reading the editor", rs_context)
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

editor_is_active = function(file, ctx) {
  !is.null(ctx) && same_path(ctx[["path"]], file)
}

# NULL means the selection is unknown (no IDE), "" that the served file is
# not the active document, so clients clear the highlight.
editor_selection = function(file, ctx, backend = editor_backend()) {
  if (identical(backend, "none"))
    return(NULL)

  if (!editor_is_active(file, ctx))
    return("")

  extract_line_nums(ctx[["selection"]])
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

# Saves the served file. Returns TRUE if a save was requested, FALSE if it
# was skipped or failed; never errors.
editor_save = function(file, id, ctx, backend = editor_backend()) {
  switch(
    backend,
    rstudio = {
      if (is.null(id))
        return(FALSE)
      isTRUE(editor_try("Auto-save", function() {
        rs_save(id)
        TRUE
      }, default = FALSE))
    },
    positron = positron_save(file, ctx),
    FALSE
  )
}

# Positron can only save the active editor, so the served file is saved
# only while it is the active document, and only when the buffer differs
# from the file (save without formatting writes even a clean buffer).
positron_save = function(file, ctx) {
  if (!editor_is_active(file, ctx))
    return(FALSE)

  if (buffer_matches_file(file, ctx[["contents"]]))
    return(FALSE)

  if (!isFALSE(editor_state$save_unformatted)) {
    ok = tryCatch(
      {
        ps_execute_command("workbench.action.files.saveWithoutFormatting")
        TRUE
      },
      error = function(e) FALSE
    )
    editor_state$save_unformatted = ok
    if (ok)
      return(TRUE)
    positron_format_note()
  }

  isTRUE(editor_try("Auto-save", function() {
    rs_save(NULL)
    TRUE
  }, default = FALSE))
}

buffer_matches_file = function(file, contents) {
  if (!is.character(contents))
    return(FALSE)

  tryCatch(
    {
      size = file.size(file)
      disk = if (size == 0) "" else readChar(file, size, useBytes = TRUE)
      Encoding(disk) = "UTF-8"
      disk = gsub("\r\n", "\n", disk, fixed = TRUE)
      identical(enc2utf8(paste(contents, collapse = "\n")), disk)
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

# The path RStudio has for the document with this id, or NULL if unknown.
editor_document_path = function(id, backend = editor_backend()) {
  if (!identical(backend, "rstudio") || is.null(id))
    return(NULL)

  path = tryCatch(rs_document_path(id), error = function(e) NULL)
  if (is_saved_path(path)) path else NULL
}

# Warns about editor settings that rewrite the file when auto-save saves it.
editor_save_warnings = function(backend = editor_backend()) {
  switch(
    backend,
    rstudio = rstudio_whitespace_warning(),
    positron = {
      if (is.null(ps_command_fun()))
        editor_state$save_unformatted = FALSE
      cli::cli_alert_info(paste(
        "In Positron livecode can only save the active editor, so the file is",
        "saved only while it is the active tab."
      ))
      if (isFALSE(editor_state$save_unformatted))
        positron_format_note()
    },
    NULL
  )
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

  cli::cli_alert_warning(paste(
    "You are running livecode with {.code auto_save = TRUE} and the {.val {opt_name}}",
    "option checked in RStudio. This can result in undesirable behavior while you broadcast.",
    "To resolve this, from RStudio's menu select {.field {menu}} and uncheck {.val {opt_name}}."
  ))
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

# Positron's settings cannot be read from R, so name the ones that matter.
positron_format_note = function() {
  if (isTRUE(editor_state$format_note))
    return(invisible())
  editor_state$format_note = TRUE

  cli::cli_alert_warning(paste(
    "livecode cannot save without formatting in this Positron session, so",
    "auto-save runs your save actions. If {.field editor.formatOnSave} or",
    "{.field files.trimTrailingWhitespace} is on, turn it off while you broadcast."
  ))
  invisible()
}

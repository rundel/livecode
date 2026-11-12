# The page highlights code with Prism, vendored in inst/resources/prism (see
# its README for the files and how to update them).

prism_version = function() {
  "1.29.0"
}

# Language components in load order: each comes after every component it
# extends.
prism_components = function() {
  c(
    "markup", "css", "clike", "javascript", "typescript", "c", "cpp", "java",
    "go", "rust", "json", "yaml", "markdown", "r", "python", "bash", "sql",
    "latex", "ini", "toml", "julia", "docker", "makefile"
  )
}

# Scripts of the code view in load order, relative to inst/resources.
prism_scripts = function() {
  c(
    "prism/prism-config.js",
    "prism/prism-core.min.js",
    paste0("prism/components/prism-", prism_components(), ".min.js"),
    "prism/livecode-qmd.js",
    "prism/livecode-code-view.js"
  )
}

# The Prism language for a file, from its extension or name. Anything not
# covered is shown as plain text ("none").
file_code_language = function(path) {
  name = tolower(basename(path))
  if (name == "dockerfile")
    return("docker")
  if (name %in% c("makefile", "gnumakefile"))
    return("makefile")

  ext = if (grepl(".", name, fixed = TRUE)) sub("^.*\\.", "", name) else ""

  switch(
    ext,
    r = "r", py = "python", qmd = , rmd = "qmd", md = , markdown = "markdown",
    sh = , bash = , zsh = "bash", sql = "sql", json = "json",
    yaml = , yml = "yaml", toml = "toml", ini = , cfg = "ini",
    js = , mjs = "javascript", ts = "typescript", css = "css",
    html = , htm = , xml = , svg = "markup",
    c = , h = "c", cpp = , cc = , hpp = "cpp", java = "java",
    jl = "julia", rs = "rust", go = "go", tex = "latex",
    "none"
  )
}

# The page every client loads. The language is also sent in the connect
# frame, this only sets the initial value. A follower's page shows messages
# as plain text (message_mode = "text") because their text comes from
# another machine.
livecode_page = function(file, lang = file_code_language(file), message_mode = c("html", "text")) {
  message_mode = match.arg(message_mode)
  scripts = paste0(
    "<script src=\"web/", prism_scripts(), "\"></script>",
    collapse = "\n    "
  )

  glue::glue(
    readr::read_file(pkg_resource("templates", "page.html", must_work = TRUE)),
    title = html_escape(basename(file)),
    lang = html_escape(lang),
    prism_scripts = scripts,
    message_mode = message_mode,
    .trim = FALSE
  )
}

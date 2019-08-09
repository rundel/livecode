pkg_resource = function(..., must_work = FALSE) {
  system.file(..., package = "livecode", mustWork = must_work)
}

get_template = function(name) {
  if (file.exists(name))
    return(name)

  file = pkg_resource("templates", paste0(name, ".html"))

  if (file == "")
    usethis::ui_stop("Template {usethis::ui_value(name)} does not exist.")

  file
}

#'
#' @export
#'
serve_file = function(file, template = "plain") {

  if (missing(file) && requireNamespace('rstudioapi', quietly = TRUE)) {
    file = rstudioapi::getSourceEditorContext()[['path']]

    if (is.null(file) || file == '')
      usethis::ui_stop('Cannot find an open document in the RStudio editor')
  }

  name = basename(file)

  d = file.path(tempdir(), name)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)


  tmpl = readr::read_file(get_template(template))
  html = file.path(d, "index.html")

  rebuild = function() {
    code = readr::read_file(file)
    title = name
    lang = "r"
    readr::write_file(glue::glue(tmpl), html)
  }


  build = local({
    d = dirname(file)
    files = file

    mtime = function() file.info(file)[, 'mtime']

    rebuild()
    l = mtime()

    r = servr:::is_rstudio();

    function(message) {
      if (length(message) != 0)
        cat("Message: ", message, "\n")
      m2 = mtime()
      if (m2 > l) {
        rebuild()
        l <<- mtime()
        TRUE
      } else {
        FALSE
      }
    }
  })

  dynamic_site(
    dir = d,
    static_paths = list("/prism" = pkg_resource("resources", "prism")),
    build = build,
    ws_handler = pkg_resource('resources', 'ws-handler.js'),
    interval = 0.1
  )
}



FileCache = R6::R6Class(
  "FileCache",
  public = list(
    initialize = function(path) {
      path = normalizePath(path)
      if (!file.exists(path))
        usethis::ui_stop("Unable to locate file {usethis::ui_value(path)}")

      private$path = path
      self$update_content()
    },
    need_update = function() {
      cur_mtime = file.mtime(private$path)
      cur_mtime > private$mtime
    },
    update_content = function() {
      #message("Updating content")
      private$mtime = file.mtime(private$path)
      private$cache_content = readr::read_file(private$path)
      self
    }
  ),
  private = list(
    path = NULL,
    mtime = NULL,
    cache_content = NULL
  ),
  active = list(
    content = function() {
      if (self$need_update()) {
        self$update_content()
      }
      private$cache_content
    }
  )
)

#' @export
file_cache = function(path) {
  FileCache$new(path)
}

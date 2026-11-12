#' File cache for tracking file changes
#'
#' @description
#' An R6 class that caches the contents of a file and updates the cache
#' only when the file changes on disk. A change is any difference in
#' modification time or size, so a file restored with an older modification
#' time is picked up too. If the file is temporarily missing (for example
#' while an editor replaces it) the last cached contents are kept.
#'
#' @export
FileCache = R6::R6Class(
  "FileCache",
  public = list(

    #' @description
    #' Create a new FileCache object.
    #'
    #' @param path Path to a file.
    initialize = function(path) {
      if (!file.exists(path))
        cli::cli_abort("Unable to locate file {.file {path}}.")

      private$path = normalizePath(path)
      self$update_content()
    },

    #' @description
    #' Determine whether the file has changed since the last cache update.
    #'
    #' @return Logical scalar. `FALSE` when the file cannot currently be read.
    need_update = function() {
      info = file.info(private$path, extra_cols = FALSE)
      if (is.na(info[["mtime"]]))
        return(FALSE)

      !identical(info[["mtime"]], private$mtime) ||
        !identical(info[["size"]], private$size)
    },

    #' @description
    #' Refresh the cached file contents. Does nothing if the file is missing.
    #'
    #' @return
    #' Returns the object invisibly.
    update_content = function() {
      info = file.info(private$path, extra_cols = FALSE)
      if (is.na(info[["mtime"]]))
        return(invisible(self))

      private$cache_content = readr::read_file(private$path)
      private$mtime = info[["mtime"]]
      private$size = info[["size"]]
      invisible(self)
    }
  ),

  private = list(
    path = NULL,
    mtime = NULL,
    size = NULL,
    cache_content = NULL
  ),

  active = list(

    #' @field content
    #' Cached file contents. Automatically refreshes if the file has changed.
    content = function() {
      if (self$need_update()) {
        self$update_content()
      }

      private$cache_content
    }
  )
)

#' Create a file cache
#'
#' @description
#' Convenience wrapper for \code{FileCache$new()}.
#'
#' @param path Path to a file.
#'
#' @return
#' A \code{FileCache} R6 object.
#'
#' @examples
#' f = tempfile(fileext = ".R")
#' writeLines("x = 1", f)
#' fc = file_cache(f)
#' fc$content
#'
#' @seealso
#' \code{\link{FileCache}}
#'
#' @export
file_cache = function(path) {
  FileCache$new(path)
}

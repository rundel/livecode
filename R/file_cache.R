#' File cache for tracking file changes
#' 
#' @description
#' An R6 class that caches the contents of a file and updates the cache
#' only when the file changes on disk.
#'
#' @export
FileCache = R6::R6Class(
  "FileCache",
  public = list(
    
    #' @description
    #' Create a new FileCache object.
    #'
    #' @param path Path to a file.
    #' @param file_id Optional file identifier.
    initialize = function(path, file_id = NULL) {
      path = normalizePath(path)
      
      if (!file.exists(path))
        usethis::ui_stop("Unable to locate file {usethis::ui_value(path)}")
      
      private$path = path
      private$file_id = file_id
      self$update_content()
    },
    
    #' @description
    #' Determine whether the file has changed since the last cache update.
    #'
    #' @return Logical scalar.
    need_update = function() {
      cur_mtime = file.mtime(private$path)
      cur_mtime > private$mtime
    },
    
    #' @description
    #' Refresh the cached file contents.
    #'
    #' @return
    #' Returns the object invisibly.
    update_content = function() {
      private$mtime = file.mtime(private$path)
      private$cache_content = readr::read_file(private$path)
      self
    }
  ),
  
  private = list(
    path = NULL,
    file_id = NULL,
    mtime = NULL,
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
#' @param file_id Optional editor file identifier.
#'
#' @return
#' A \code{FileCache} R6 object.
#'
#' @examples
#' \dontrun{
#' fc <- file_cache("script.R")
#' fc$content
#' }
#' @seealso
#' \code{\link{FileCache}}
#'
#' @export
file_cache = function(path, file_id = NULL) {
  FileCache$new(path, file_id)
}

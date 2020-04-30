FileCache = R6::R6Class(
  "FileCache",
  public = list(
    initialize = function(path, file_id = NULL, auto_save = TRUE) {
      path = normalizePath(path)
      if (!file.exists(path))
        usethis::ui_stop("Unable to locate file {usethis::ui_value(path)}")
      
      self$set_auto_save(auto_save)
      self$set_file(path, file_id)
    },
    
    set_file = function(path, file_id = NULL) {
      prev_file = private$path %||% ""
      private$path = path
      private$file_id = file_id
      self$update_content()
      
      if (prev_file != path) # If new file, force an update on the next check
        private$mtime = 0
    },
    
    set_auto_save = function(flag) {
      private$auto_save = flag
    },
    
    file_changed = function() {
      cur_mtime = file.mtime(private$path)
      cur_mtime > private$mtime
    },
    
    update_content = function() {
      private$mtime = file.mtime(private$path)
      private$cache_content = readr::read_file(private$path)
      self
    },
    
    save = function() {
      if (!is.null(private$file_id) && private$auto_save)
        rstudioapi::documentSave(private$file_id)
      
      invisible()
    },
    
    get_content = function() {
      if (self$file_changed()) {
        self$update_content()
      }
      private$cache_content
    },
    
    get_path = function() {
      private$path
    },
    
    get_filename = function() {
      fs::path_file(private$path)
    }
  ),
  private = list(
    path = NULL,
    file_id = NULL,
    auto_save = NULL,
    mtime = NULL,
    cache_content = NULL
  )
)

file_cache = function(path, file_id = NULL, auto_save = TRUE) {
  FileCache$new(path, file_id, auto_save)
}

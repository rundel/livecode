get_private_member = function(obj, member) {
  obj[[".__enclos_env__"]][["private"]][[member]]
}

FileStreamServer <- R6::R6Class(
  "FileStreamServer",
  cloneable = FALSE,
  inherit = httpuv:::WebServer,
  public = list(
    initialize = function(host, port, file, interval = 2.5, template = "progress") {

      private$file_cache = file_cache(file)
      private$interval = interval
      private$page = glue::glue(
        readr::read_file(get_template(template)),
        lang = "r",
        title = file
      )

      FileStreamApp <- list(
        call = function(req) {
          list(
            status = 200L,
            headers = list(
              'Content-Type' = 'text/html'
            ),
            body = private$page
          )
        },
        onWSOpen = function(ws) {
          msg = list(
            interval = private$interval,
            content = private$file_cache$content
          )

          ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))

          NULL
        },
        staticPaths = list(
          "/web" = pkg_resource("resources")
        )
      )

      super$initialize(host, port, FileStreamApp)

      private$ws = get_private_member(private$appWrapper, "wsconns")

      send_and_reschedule <- function() {
        if (!self$isRunning())
          return()

        message(Sys.time())

        for(n in names(private$ws)) {
          if (is.null(private$ws[[n]]))
            next
          msg = list(
            interval = private$interval
          )

          if (private$file_cache$need_update()) {
            msg$content = private$file_cache$content
          }

          private$ws[[n]]$send(jsonlite::toJSON(msg, auto_unbox = TRUE))
        }

        later::later(send_and_reschedule, private$interval)
      }
      send_and_reschedule()
    }
  ),
  private = list(
    ws = NULL,
    file_cache = NULL,
    interval = NULL,
    page = NULL
  )
)

#' @export
serve_file = function() {
  later::later(~browseURL("http://localhost:5000/", browser = get_browser()), 1)
  FileStreamServer$new("0.0.0.0", 5000L, "reprex.R")
}

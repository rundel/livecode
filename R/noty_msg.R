noty_msg = R6::R6Class(
  "noty_msg",
  public = list(
    initialize = function(
      text,
      type = c("alert", "success", "error", "warning", "info"),
      layout = c("topRight", "top", "topLeft", "topCenter",
                 "center", "centerLeft", "centerRight",
                 "bottom", "bottomLeft", "bottomCenter", "bottomRight"),
      theme = c("mint", "sunset", "relax",
                "nest", "metroui", "semanticui",
                "light", "bootstrap-v3", "bootstrap-v4"),
      timeout = 10000,
      progressBar = FALSE,
      ...
    ) {
      private$text = text
      private$args = c(
        list(
          type = match.arg(type),
          layout = match.arg(layout),
          theme = match.arg(theme),
          timeout = timeout,
          progressBar = progressBar
        ),
        list(...)
      )
    },
    print = function(...) {
      text = private$text
      cli::cli_text("{.strong Noty Message:} {.val {text}}")
      args = vapply(private$args, function(x) paste(format(x), collapse = ", "), character(1))
      args = cli_escape(args)
      names(args) = cli_escape(names(private$args))
      cli::cli_dl(args)
      invisible(self)
    },
    get_msg = function() {
      c(
        list(text = private$text),
        private$args
      )
    },
    get_text = function() {
      private$text
    }
  ),
  private = list(
    text = character(),
    args = list()
  )
)

noty_msg <- R6::R6Class(
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
      timeout = 0,
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
      usethis:::cat_line(
        crayon::bold("Noty Message: "),
        usethis::ui_value(private$text)
      )
      purrr::walk2(
        names(private$args), private$args,
        ~usethis:::kv_line(.x, .y)
      )
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
    #closeWith = character()
  )
)




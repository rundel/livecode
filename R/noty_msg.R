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
      progressBar = FALSE
      #closeWith = c("click", "button")
    ) {
      private$text = text
      private$type = match.arg(type)
      private$layout = match.arg(layout)
      private$theme = match.arg(theme)
      private$timeout = timeout
      private$progressBar = progressBar
      #private$closeWith = match.arg(closeWith, several.ok = TRUE)
    },
    print = function(...) {
      usethis:::cat_line(
        crayon::bold("Noty Message: "),
        usethis::ui_value(private$text)
      )
      usethis:::kv_line("Type", private$type)
      usethis:::kv_line("Layout", private$layout)
      usethis:::kv_line("Theme", private$theme)
      usethis:::kv_line("Timeout", private$timeout)
      usethis:::kv_line("Prorgessbar", private$progressBar)
      #usethis:::kv_line("Closewith", private$closeWith)
    },
    get_msg = function() {
      as.list(private)
    },
    get_text = function() {
      private$text
    }
  ),
  private = list(
    type = character(),
    layout = character(),
    theme = character(),
    text = character(),
    timeout = integer(),
    progressBar = logical()
    #closeWith = character()
  )
)




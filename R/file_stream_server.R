lc_server <- R6::R6Class(
  "LiveCodeServer",

  cloneable = FALSE,
  inherit = httpuv:::WebServer,
  public = list(
    have_msgs = function() {
      length(private$msg_queue) > 0
    },

    get_msgs = function() {
      msgs = private$msg_queue
      private$msg_queue = list()
      msgs
    },

    peek_msgs = function() {
      purrr::map_chr(private$msg_queue, ~ .$get_text())
    },

    add_msg = function(m) {


      if (is.character(m) & length(m) == 1) {
        m = noty_msg$new(m)
      }

      if (!inherits(m, "noty_msg")) {
        usethis::ui_stop("Invalid message type, must either be a character or noty_msg object.")
      }
      private$msg_queue = append(private$msg_queue, m)
    }
  ),
  private = list(
    msg_queue = list()
  )
)


file_stream_server = function(host, port, file, file_id, interval = 3, template = "prism") {
  port = as.integer(port)
  file_cache = file_cache(file)
  page = glue::glue(
    readr::read_file(get_template(template)),
    lang = "r",
    title = file
  )

  get_next_ws_id = local({
    next_ws_id = 0L
    function() {
      sprintf("%012d", next_ws_id <<- next_ws_id + 1L)
    }
  })

  websockets = new.env(parent = emptyenv())

  websocket_loop = function() {
    if (!server$isRunning())
      return()

    if (is_rstudio() & !is.null(file_id)) {
      rstudioapi::documentSave(file_id)
    }

    msg = list(interval = interval)
    if (file_cache$need_update())
      msg[["content"]] = file_cache$content

    if (server$have_msgs()) {
      msgs = purrr::map(server$get_msgs(), ~ .$get_msg())

      msg[["messages"]] = msgs
    }

    if (is_rstudio()) {
      ctx = rstudioapi::getSourceEditorContext()
      open_file = path.expand(ctx[["path"]])

      if (file == open_file) {
        ln = extract_line_nums(ctx[["selection"]])
        msg[["selection"]] = ln
      }
    }

    msg = jsonlite::toJSON(msg, auto_unbox = TRUE)

    for(ws_id in names(websockets)) {
      websockets[[ws_id]]$send(msg)
    }

    later::later(websocket_loop, interval)
  }

  app = list(
    call = function(req) {
      list(
        status = 200L,
        headers = list(
          #'Content-Type' = 'text/html'
          'Content-Type'='text/html; charset=UTF-8'
        ),
        body = page
      )
    },

    onWSOpen = function(ws) {
      ws_id = get_next_ws_id()
      websockets[[ws_id]] = ws

      ws$onClose(
        function() {
          rm(list = ws_id, envir = websockets)
        }
      )

      ## Send initial message with current file contents
      msg = list(
        interval = interval,
        content = file_cache$content
      )
      ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))

      if (as.integer(ws_id) == 1)
        websocket_loop()
    },

    staticPaths = list(
      "/web" = livecode:::pkg_resource("resources")
    )
  )

  # Must be defined for the websocket_loop above to work
  server = lc_server$new(host, port, app)

  server
}

#' livecode server interface
#'
#' @description
#' This is a high level, user facing interface class that allows
#' for the creation of a livecode server sharing a specific file.
#' The interface also provides additional tools for sending messages.
#'
#' @export

lc_server_iface = R6::R6Class(
  "LiveCodeServer_Interface",
  cloneable = FALSE,
  private = list(
    file = NULL,
    file_id = NULL,
    ip = NULL,
    port = NULL,
    template = NULL,
    interval = NULL,
    bitly_url = NULL,
    server = NULL,

    init_file = function(file, auto_save) {

      if (missing(file))
        file = NULL
      else if (!is.null(file))
        file = path.expand(file)

      file_id = NULL
      if (is_rstudio()) {
        if (is.character(file)) {
          rstudioapi::navigateToFile(file)
          Sys.sleep(0.5)
        }

        ctx = rstudioapi::getSourceEditorContext()

        file = path.expand(ctx[["path"]])
        file_id = ctx[["id"]]
      }

      if (!auto_save)
        file_id = NULL

      if (is.null(file) | file == "") {
        usethis::ui_stop( paste(
          "No file specified, if you are using RStudio ",
          "make sure the current open file has been saved ",
          "at least once."
        ) )
      }

      private$file = file
      private$file_id = file_id
    },

    init_ip = function(ip) {
      if (missing(ip)) {
        ip = network_interfaces()[["ip"]][1]

        #usethis::ui_info( c(
        #  "No ip address provided, using {usethis::ui_value(ip)}",
        #  "(If this does not work check available ips using {usethis::ui_code(\"network_interfaces()\")})"
        #))
      }

      if (is.na(iptools::ip_classify(ip))) {
        usethis::ui_stop( paste(
          "Invalid ip address provided ({usethis::ui_value(ip)})."
        ) )
      }

      private$ip = ip
    },

    init_port = function(port) {
      if (missing(port)) {
        port = httpuv::randomPort(host = private$ip)
        #usethis::ui_info( paste(
        #  "No port provided, using port {usethis::ui_value(port)}."
        #))
      }

      port = as.integer(port)

      if (port < 1024L | port > 49151L) {
        usethis::ui_stop( paste(
          "Invalid port ({usethis::ui_value(ip)}), value must be between 1024 and 49151."
        ) )
      }

      private$port = port
    },

    init_bitly = function() {
      res = purrr::safely(bitly_shorten)(self$url)
      if (succeeded(res)) {
        private$bitly_url = result(res)
      } else {
        usethis::ui_oops( paste0(
          "Failed to create bitlink: ",
          error_msg(res)
        ) )
      }
    },

    init_auto_save = function() {
      if (!check_strip_trailing_ws())
        return()

      opt_name = usethis::ui_value('Strip trailing horizontal whitespace when saving')
      if (using_project())
        menu = "Tools > Project Options > Code Editing"
      else
        menu = "Tools > Global Options > Code > Saving"

      usethis::ui_oops( paste(
        "You are running livecode with {usethis::ui_code('auto_save=TRUE')} with the {opt_name}",
        "option checked in RStudio.\n This can result in undesirable behavior while you broadcast.",
        "To resolve this, from RStudio's menu select:\n {menu} and uncheck {opt_name}."
      ) )
    },

    start_server = function() {
      private$server = file_stream_server(
        private$ip, private$port, private$file, private$file_id,
        template = private$template, interval = private$interval
      )

      usethis::ui_done( paste(
        "Started sharing {usethis::ui_value(fs::path_file(private$file))}",
        "at {usethis::ui_value(self$url)}."
      ) )

      if (is_ip_private(private$ip)) {
        usethis::ui_oops( paste(
          "The current ip address ({usethis::ui_value(private$ip)}) for the server is private,",
          "only users on the same local network are likely to be able to connect."
        ) )
      }


      later::later(~self$open(), 1)
    }

  ),
  public = list(
    #' @description
    #' Creates a new livecode server
    #'
    #' @param file Path to file to broadcast.
    #' @param ip ip of the server, defaults to the top result of `network_interfaces`.
    #' @param port port of the server, defaults to a random value.
    #' @param interval page update interval in seconds.
    #' @param template page template to use.
    #' @param bitly should a bitly bit link be created for the server.
    #' @param auto_save should the broadcast file be auto saved update tic.
    initialize = function(
      file, ip, port, interval = 2, template = "prism",
      bitly = FALSE, auto_save = TRUE
    ) {
      private$init_file(file, auto_save)
      private$init_ip(ip)
      private$init_port(port)

      private$template = template
      private$interval = interval
      private$start_server()

      if (bitly)
        private$init_bitly()

      if (auto_save)
        private$init_auto_save()
    },

    #' @description
    #' Open server in browser
    open = function() {
      if (self$is_running())
        browseURL(self$url, browser = get_browser())
      else
        usethis::ui_stop("The server is not currently running!")
    },

    #' @description
    #' Class print method
    print = function() {
      usethis:::cat_line(
        crayon::bold("File Streaming Server: "),
        ifelse(
          private$server$isRunning(),
          "(running)",
          "(stopped)"
        )
      )
      usethis:::kv_line("file", private$file)

      if (private$server$isRunning()) {
        usethis:::kv_line("url", self$url)
        usethis:::kv_line("ip type", as.character(ip_type(private$ip)))
        if (!is.null(private$bitly_url))
          usethis:::kv_line("bitly url", private$bitly_url)
      }
    },

    #' @description
    #' Send a noty message to all connected users on the next update tic.
    #'
    #' @param text text of the message.
    #' @param type message type (`alert`, `success`, `warning`, `error`, `info`).
    #' @param theme message theme (See [here](https://ned.im/noty/#/themes) for options)
    #' @param layout message location.
    #' @param ... addition noty arguments.
    #' @param parse_md should message text be processed as markdown before sending.
    send_msg = function(text,
                        type = "info",
                        theme = "bootstrap-v4",
                        layout = "topRight",
                        ...,
                        parse_md = TRUE) {
      if (parse_md) {
        text = markdown::markdownToHTML(
          text = text,
          fragment.only = TRUE,
          extensions = markdown::markdownExtensions()
        )
      } else {
        text = paste(text, collapse = "\n")
      }

      args = c(
        list(text = text, type = type, theme = theme, layout = layout),
        list(...)
      )

      text_has_link = grepl("<a ", text)
      closeWith_used = "closeWith" %in% names(args)

      # Message closes with a button click
      if (text_has_link & !closeWith_used)
        args[["closeWith"]] = list("button")


      private$server$add_msg(
        do.call(noty_msg$new, args)
      )
    },

    #' @description
    #' Determine if the server is running.
    #' @return Returns `TRUE` if the server is running.
    is_running = function() {
      private$server$isRunning()
    },

    #' @description
    #' Stop the server
    #'
    #' @param warn Should the users be sent a warning that the server is shutting down.
    stop = function(warn = FALSE) {
      if (warn) {
        self$send_msg("Server is shutting down!", type = "error")
        Sys.sleep(private$interval)
        later::run_now()
      }
      # Wait for a tic before shutting down so the message will go out
      Sys.sleep(private$interval)

      private$server$stop()

      usethis::ui_done( paste(
        "Stopped server at {usethis::ui_value(self$url)}."
      ) )
    },

    #' @description
    #' Restart the server
    restart = function() {
      if (self$is_running()) {
        self$stop()
      }

      private$start_server()
    }
  ),
  active = list(
    #' @field url The current url of the server.
    url = function() {
      if (!is.null(private$bitly_url))
        private$bitly_url
      else
        glue::glue("http://{private$ip}:{private$port}")
    },
    #' @field path The path of the file being served.
    path = function() {
      private$file
    }
  )
)

#' Create a livecode server for broadcasting a file
#'
#' @param file Path to file to broadcast.
#' @param ip ip of the server, defaults to the top result of `network_interfaces`.
#' @param port port of the server, defaults to a random value.
#' @param bitly should a bitly bit link be created for the server.
#' @param auto_save should the broadcast file be auto saved update tic.
#' @param interval page update interval in seconds.
#' @param template page template to use.
#'
#' @export

serve_file = function(file, ip, port, bitly = FALSE, auto_save = TRUE, template = "prism", interval = 1) {
  z = lc_server_iface$new(file, ip, port, interval, template, bitly, auto_save)

  z$send_msg(
    text = welcome_msg(z)
  )

  z
}

welcome_msg = function(server) {
  c(
    "## Welcome to `livecode`!",
    "",
    glue::glue("Serving `{fs::path_file(server$path)}` at"),
    "",
    glue::glue(
      "<div class='server_link'>",
      "<a href='{server$url}'>",
      "{server$url}",
      "</a>",
      "</div>"
    )
  )
}


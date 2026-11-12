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


#' Create a low-level livecode file streaming server
#'
#' @description
#' Creates and returns a low-level HTTP/WebSocket server used by
#' \pkg{livecode} to broadcast the contents of a source file to connected
#' browsers.
#'
#' The server hosts an HTML page, serves supporting static assets, and maintains
#' WebSocket connections to all connected clients. File changes, editor cursor
#' selections, and queued messages are pushed to clients at a fixed interval.
#'
#' This function is primarily an internal constructor used by
#' \code{\link{serve_file}} and \code{LiveCodeServer_Interface}.
#'
#' @param host Character string giving the IP address or hostname on which the
#'   server should listen.
#' @param port Integer port number for the server.
#' @param file Path to the file being broadcast.
#' @param file_id Optional editor-specific file identifier used by RStudio for
#'   auto-save operations.
#' @param interval Numeric update interval in seconds between broadcast ticks.
#' @param template Character string naming the HTML template to use. Defaults
#'   to \code{"prism"}.
#'
#' @details
#' The server performs the following tasks:
#'
#' \itemize{
#'   \item Serves a rendered HTML page based on the selected template.
#'   \item Tracks the source file using \code{\link{file_cache}}.
#'   \item Pushes updated file contents to clients when the file changes.
#'   \item Pushes queued messages from the server message queue.
#'   \item In RStudio, optionally saves the file on each update tick.
#'   \item In RStudio, broadcasts the current editor selection when the served
#'     file is the active document.
#'   \item Serves static assets (JavaScript, CSS, etc.) from the package
#'     resources directory under \code{/web}.
#' }
#'
#' Connected clients receive JSON messages over WebSocket containing one or more
#' of:
#'
#' \itemize{
#'   \item \code{content}: updated file contents
#'   \item \code{messages}: queued notifications
#'   \item \code{selection}: highlighted line selection
#'   \item \code{interval}: refresh interval
#' }
#'
#' @return
#' A \code{LiveCodeServer} R6 object inheriting from
#' \code{httpuv:::WebServer}.
#'
#' @seealso
#' \code{\link{serve_file}},
#' \code{\link{file_cache}},
#' \code{\link{lc_server_iface}}
#'
#' @keywords internal
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
          'Content-Type' = 'text/html; charset=UTF-8'
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
      "/web" = pkg_resource("resources")
    )
  )

  # Must be defined for the websocket_loop above to work
  server = lc_server$new(host, port, app)

  server
}

#' Livecode Server Interface
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
        usethis::ui_stop(paste(
          "No file specified, if you are using RStudio ",
          "make sure the current open file has been saved ",
          "at least once."
        ))
      }
      
      private$file = file
      private$file_id = file_id
    },
    
    init_ip = function(ip) {
      if (missing(ip)) {
        ip = network_interfaces()[["ip"]][1]
      }
      
      if (is_ipv6(ip)) {
        usethis::ui_stop(paste(
          "IPv6 addresses ({usethis::ui_value(ip)}) are not supported, use an IPv4 address."
        ))
      }

      if (length(ip) != 1 || !is_ipv4(ip)) {
        usethis::ui_stop(paste(
          "Invalid ip address provided ({usethis::ui_value(ip)})."
        ))
      }
      
      private$ip = ip
    },
    
    init_port = function(port) {
      if (missing(port)) {
        port = httpuv::randomPort(host = private$ip)
      }
      
      port = as.integer(port)
      
      if (port < 1024L | port > 49151L) {
        usethis::ui_stop(paste(
          "Invalid port ({usethis::ui_value(port)}), value must be between 1024 and 49151."
        ))
      }
      
      private$port = port
    },
    
    init_auto_save = function() {
      if (!check_strip_trailing_ws())
        return()
      
      opt_name = usethis::ui_value("Strip trailing horizontal whitespace when saving")
      if (using_project())
        menu = "Tools > Project Options > Code Editing"
      else
        menu = "Tools > Global Options > Code > Saving"
      
      usethis::ui_oops(paste(
        "You are running livecode with {usethis::ui_code('auto_save=TRUE')} with the {opt_name}",
        "option checked in RStudio. This can result in undesirable behavior while you broadcast.\n",
        "To resolve this, from RStudio's menu select:\n {menu} and uncheck {opt_name}."
      ))
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
    #' @param auto_save should the broadcast file be auto saved update tic.
    #' @param open_browser should a browser session be opened.
    initialize = function(
    file, ip, port, interval = 2,
    auto_save = TRUE, open_browser = TRUE
    ) {
      private$init_file(file, auto_save)
      private$init_ip(ip)
      private$init_port(port)
      
      private$template = "prism"
      private$interval = interval
      self$start()
      
      if (auto_save)
        private$init_auto_save()
      
      if (open_browser)
        later::later(~self$open(), 1)
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
      usethis::ui_line(paste(
        crayon::bold("livecode server:"),
        crayon::red(basename(private$file)),
        "@",
        crayon::underline(crayon::blue(self$url))
      ))
    },
    
    #' @description
    #' Send a noty message to all connected users on the next update tic.
    #'
    #' @param text text of the message.
    #' @param type message type (`alert`, `success`, `warning`, `error`, `info`).
    #' @param theme message theme (See [here](https://ned.im/noty/#/themes) for options)
    #' @param layout message location.
    #' @param ... additional noty arguments.
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
          fragment.only = TRUE
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
    #' Start the server
    start = function() {
      private$server = file_stream_server(
        private$ip, private$port, private$file, private$file_id,
        template = private$template, interval = private$interval
      )
      
      usethis::ui_done(paste(
        "Started sharing {usethis::ui_value(basename(private$file))}",
        "at {usethis::ui_value(self$url)}."
      ))
      
      if (is_ip_private(private$ip)) {
        usethis::ui_oops(paste(
          "The current ip address ({usethis::ui_value(private$ip)}) for the server is private,",
          "only users on the same local network are likely to be able to connect."
        ))
      }
      
      register_server(self)
      invisible(self)
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
      
      Sys.sleep(private$interval)
      
      if (!is.null(private$server) && private$server$isRunning()) {
        private$server$stop()
      }
      
      usethis::ui_done(paste(
        "Stopped server at {usethis::ui_value(self$url)}."
      ))
      
      deregister_server(self)
      invisible(self)
    },
    
    #' @description
    #' Restart the server
    restart = function() {
      if (self$is_running()) {
        self$stop()
      }
      
      self$start()
      
      invisible(self)
    }
  ),
  active = list(
    #' @field url The current url of the server.
    url = function() {
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
#' @description
#' Starts a local \code{livecode} server that streams a syntax-highlighted
#' version of a source file and automatically updates it in connected browsers.
#' The server uses a WebSocket connection to push updates at a fixed interval.
#'
#' @param file Path to the file to broadcast. If not provided and running in
#'   RStudio, the currently active document is used.
#' @param ip IP address for the local server. Defaults to the first available
#'   network interface (see \code{network_interfaces()}).
#' @param port Port for the local server. Defaults to a random available port.
#' @param interval Numeric. Update interval in seconds for refreshing content
#'   and pushing updates to clients.
#' @param auto_save Logical. Should the source file be automatically saved at
#'   each update interval (RStudio only).
#' @param open_browser Logical. Should a browser session be opened automatically.
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' The server is implemented using \pkg{httpuv} and maintains an open WebSocket
#' connection to all clients. Updates are pushed at the specified \code{interval},
#' including:
#' \itemize{
#'   \item Updated file contents
#'   \item Cursor/selection position (RStudio only)
#'   \item Messages sent via \code{send_msg()}
#' }
#'
#' @return
#' An invisible \code{LiveCodeServer_Interface} R6 object representing the running
#' server. This object provides methods such as:
#' \itemize{
#'   \item \code{$stop()} to stop the server
#'   \item \code{$restart()} to restart the server
#'   \item \code{$send_msg()} to send messages to connected clients
#' }
#'
#' @examples
#' \dontrun{
#' srv <- serve_file("script.R")
#'
#' srv$send_msg("Hello, everyone!", type = "success")
#'
#' srv$stop()
#' }
#'
#' @export
serve_file = function(file,
                      ip,
                      port,
                      interval = 1,
                      auto_save = TRUE,
                      open_browser = TRUE,
                      ...) {
  server <- lc_server_iface$new(
    file = file, ip = ip, port = port,
    interval = interval,
    auto_save = auto_save, open_browser = open_browser
  )
  
  welcome_msg <- c(
    "## Welcome to `livecode`!",
    "",
    glue::glue("Serving `{basename(server$path)}` at"),
    "",
    glue::glue(
      "<div class='server_link'><a href='{server$url}'>{server$url}</a></div>"
    )
  )
  
  server$send_msg(text = welcome_msg)
  invisible(server)
}

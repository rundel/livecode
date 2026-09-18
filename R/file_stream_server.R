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
file_stream_server = function(host, port, file, file_id,
                              interval = 3,
                              template = "prism",
                              track_selection = TRUE) {
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

    tryCatch({
      if (is_rstudio() & !is.null(file_id)) {
        rstudioapi::documentSave(file_id)
      }

      msg = list(interval = interval)

      if (file_cache$need_update()) {
        content = file_cache$content
        if (nzchar(content))
          msg[["content"]] = content
      }

      if (server$have_msgs()) {
        msgs = purrr::map(server$get_msgs(), ~ .$get_msg())
        msg[["messages"]] = msgs
      }

      if (track_selection && is_rstudio()) {
        ctx <- tryCatch(
          rstudioapi::getSourceEditorContext(),
          error = function(e) NULL
        )

        if (!is.null(ctx) && !is.null(ctx[["path"]]) && nzchar(ctx[["path"]])) {
          open_file <- path.expand(ctx[["path"]])

          if (identical(file, open_file)) {
            ln <- extract_line_nums(ctx[["selection"]])
            msg[["selection"]] <- ln
          }
        }
      }

      msg = jsonlite::toJSON(msg, auto_unbox = TRUE)

      for (ws_id in names(websockets)) {
        websockets[[ws_id]]$send(msg)
      }
    }, error = function(e) {
      message("[livecode] websocket_loop error: ", conditionMessage(e))
    })

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
      
      ws$onClose(function() {
        rm(list = ws_id, envir = websockets)
      })
      
      initial_content = file_cache$content
      msg = list(interval = interval)
      if (nzchar(initial_content))
        msg[["content"]] = initial_content
      ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))
      
      if (as.integer(ws_id) == 1)
        later::later(websocket_loop, interval)
    },
    
    staticPaths = list(
      "/web" = livecode:::pkg_resource("resources")
    )
  )
  
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
    bitly_url = NULL,
    server = NULL,
    track_selection = NULL,
    ngrok_process = NULL,
    ngrok_domain = NULL,
    ngrok_bin = NULL,
    .public_url = NULL,
    
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
      
      if (is.na(iptools::ip_classify(ip))) {
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
    
    init_bitly = function() {
      res = purrr::safely(bitly_shorten)(self$url)
      if (succeeded(res)) {
        private$bitly_url = result(res)
      } else {
        usethis::ui_oops(paste0(
          "Failed to create bitlink: ",
          error_msg(res)
        ))
      }
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
    #' @param bitly should a bitly bit link be created for the server.
    #' @param auto_save should the broadcast file be auto saved update tic.
    #' @param open_browser should a browser session be opened.
    initialize = function(
    file, ip, port, interval = 2,
    bitly = FALSE, auto_save = TRUE, open_browser = TRUE
    ) {
      private$init_file(file, auto_save)
      private$init_ip(ip)
      private$init_port(port)
      private$track_selection = !auto_save
      private$template = "prism"
      private$interval = interval
      self$start()
      
      if (bitly)
        private$init_bitly()
      
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
        crayon::red(fs::path_file(private$file)),
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
        template = private$template,
        interval = private$interval,
        track_selection = private$track_selection
      )
      
      usethis::ui_done(paste(
        "Started sharing {usethis::ui_value(fs::path_file(private$file))}",
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
      
      self$stop_ngrok()
      
      usethis::ui_done(paste(
        "Stopped server at {usethis::ui_value(self$url)}."
      ))
      
      deregister_server(self)
      invisible(self)
    },
    
    #' @description
    #' Start an ngrok tunnel for the server.
    #'
    #' @param domain Optional reserved ngrok domain.
    #' @param ngrok_bin Path to ngrok binary.
    start_ngrok = function(domain = NULL, ngrok_bin = "ngrok") {
      if (!self$is_running()) {
        usethis::ui_stop("Cannot start ngrok because the livecode server is not running.")
      }
      
      if (!is.null(private$ngrok_process) && private$ngrok_process$is_alive()) {
        usethis::ui_oops("ngrok is already running for this server.")
        return(invisible(self))
      }
      
      private$ngrok_domain <- domain
      private$ngrok_bin <- ngrok_bin
      
      args <- c("http", paste0(private$ip, ":", private$port))
      if (!is.null(domain)) {
        if (!grepl("^https?://", domain))
          domain <- paste0("https://", domain)
        args <- c(args, "--url", domain)
      }

      private$ngrok_process <- processx::process$new(
        command = ngrok_bin,
        args = args,
        stdout = "|",
        stderr = "|"
      )

      private$.public_url <- query_ngrok_public_url()

      if (is.null(private$.public_url)) {
        self$stop_ngrok()
        usethis::ui_stop("Failed to start ngrok tunnel or retrieve its public URL.")
      }

      usethis::ui_done(
        glue::glue("Started ngrok tunnel at {usethis::ui_value(private$.public_url)}.")
      )

      invisible(self)
    },
    
    #' @description
    #' Stop the ngrok tunnel if one is active.
    stop_ngrok = function() {
      if (!is.null(private$ngrok_process) && private$ngrok_process$is_alive()) {
        private$ngrok_process$kill()
        usethis::ui_done("Stopped ngrok tunnel.")
      }
      
      private$ngrok_process <- NULL
      private$.public_url <- NULL
      invisible(self)
    },
    
    #' @description
    #' Restart the server
    restart = function() {
      had_tunnel <- !is.null(private$ngrok_process) && private$ngrok_process$is_alive()
      domain <- private$ngrok_domain
      ngrok_bin <- private$ngrok_bin
      
      if (self$is_running()) {
        self$stop()
      }
      
      self$start()
      
      if (had_tunnel) {
        self$start_ngrok(domain = domain, ngrok_bin = ngrok_bin)
      }
      
      invisible(self)
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
    },
    
    #' @field public_url The current public ngrok URL, if any.
    public_url = function() {
      private$.public_url
    },
    
    #' @field tunnel_active Whether an ngrok tunnel is currently active.
    tunnel_active = function() {
      !is.null(private$ngrok_process) && private$ngrok_process$is_alive()
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
#' Optionally, an \href{https://ngrok.com}{ngrok} tunnel can be launched to expose
#' the local server to the public internet via a secure HTTPS URL.
#'
#' @param file Path to the file to broadcast. If not provided and running in
#'   RStudio, the currently active document is used.
#' @param ip IP address for the local server. Defaults to the first available
#'   network interface (see \code{network_interfaces()}).
#' @param port Port for the local server. Defaults to a random available port.
#' @param interval Numeric. Update interval in seconds for refreshing content
#'   and pushing updates to clients.
#' @param bitly Logical. Should a Bitly short link be generated for the local
#'   server URL.
#' @param auto_save Logical. Should the source file be automatically saved at
#'   each update interval (RStudio only).
#' @param open_browser Logical. Should a browser session be opened automatically.
#'   If \code{tunnel = TRUE} and a public URL is available, the browser will open
#'   the public URL; otherwise, it opens the local URL.
#' @param tunnel Logical. Should an \code{ngrok} tunnel be started to expose the
#'   local server. Requires \code{ngrok} to be installed and available on the
#'   system path.
#' @param ngrok_domain Optional character string specifying a custom ngrok domain.
#'   Requires an ngrok account with reserved domains.
#' @param ngrok_bin Character string giving the path to the \code{ngrok} binary.
#'   Defaults to \code{"ngrok"} (assumes it is available on the system path).
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
#' When \code{tunnel = TRUE}, an ngrok process is launched in the background using
#' \pkg{processx}. The process is attached to the returned server object and can
#' be stopped using \code{$stop_ngrok()} or \code{$stop()}.
#'
#' Note that when accessing the server via HTTPS (e.g., through ngrok), WebSocket
#' connections must use the \code{wss://} protocol. This is handled automatically
#' in the client JavaScript.
#'
#' @return
#' An invisible \code{LiveCodeServer_Interface} R6 object representing the running
#' server. This object provides methods such as:
#' \itemize{
#'   \item \code{$stop()} to stop the server (and any active tunnel)
#'   \item \code{$restart()} to restart the server
#'   \item \code{$send_msg()} to send messages to connected clients
#'   \item \code{$start_ngrok()} and \code{$stop_ngrok()} to manage the tunnel
#' }
#'
#' @examples
#' \dontrun{
#' srv <- serve_file("script.R")
#'
#' srv <- serve_file("script.R", tunnel = TRUE)
#'
#' srv <- serve_file(
#'   "script.R",
#'   tunnel = TRUE,
#'   ngrok_domain = "mydomain.ngrok.io"
#' )
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
                      bitly = FALSE,
                      auto_save = TRUE,
                      open_browser = TRUE,
                      tunnel = FALSE,
                      ngrok_domain = NULL,
                      ngrok_bin = "ngrok",
                      ...) {
  server <- lc_server_iface$new(
    file = file, ip = ip, port = port,
    interval = interval, bitly = bitly,
    auto_save = auto_save, open_browser = FALSE
  )
  
  if (tunnel) {
    server$start_ngrok(domain = ngrok_domain, ngrok_bin = ngrok_bin)
  }
  
  if (open_browser) {
    later::later(function() {
      browseURL(
        if (!is.null(server$public_url)) server$public_url else server$url,
        browser = get_browser()
      )
    }, 1)
  }
  
  welcome_msg <- c(
    "## Welcome to `livecode`!",
    "",
    glue::glue("Serving `{fs::path_file(server$path)}` at"),
    "",
    glue::glue(
      "<div class='server_link'><a href='{server$url}'>{server$url}</a></div>"
    )
  )
  
  if (!is.null(server$public_url)) {
    welcome_msg <- c(
      welcome_msg,
      "",
      "Public URL:",
      "",
      glue::glue(
        "<div class='server_link'><a href='{server$public_url}'>{server$public_url}</a></div>"
      )
    )
  }
  
  server$send_msg(text = welcome_msg)
  invisible(server)
}

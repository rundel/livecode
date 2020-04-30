#' livecode server interface
#'
#' @description
#' This is a high level, user facing interface class that allows
#' for the creation of a livecode server sharing a specific file.
#' The interface also provides additional tools for sending messages.
#'
#' @export

livecode_server = R6::R6Class(
  "livecode_server",
  cloneable = FALSE,
  private = list(
    file_cache = NULL,
    ip = NULL,
    port = NULL,
    template = NULL,
    interval = NULL,
    bitly_url = NULL,
    server = NULL,
    
    init_ip = function(ip) {
      if (is.null(ip)) {
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
      if (is.null(port)) {
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
        "option checked in RStudio. This can result in undesirable behavior while you broadcast.\n",
        "To resolve this, from RStudio's menu select:\n {menu} and uncheck {opt_name}."
      ) )
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
      file = NULL, ip = NULL, port = NULL, interval = 2,
      bitly = FALSE, auto_save = TRUE, open_browser = TRUE
    ) {
      self$set_file(file, auto_save)
      
      private$init_ip(ip)
      private$init_port(port)
      
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
    #' Set the file being served
    #'
    #' @param file Path to file to broadcast.
    #' @param auto_save should the broadcast file be auto saved update tic.
    set_file = function(file = NULL, auto_save = FALSE) {
      if (!is.null(file))
        file = path.expand(file)
      
      file_id = NULL
      if (is_rstudio()) {
        if (is.character(file)) {
          rstudioapi::navigateToFile(file)
          Sys.sleep(0.1)
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
      
      if (is.null(private$file_cache)) {
        # No existing file cache - create
        private$file_cache = file_cache(file, file_id, auto_save) 
      } else {
        # Change of file
        private$file_cache$set_file(file, file_id)
        private$file_cache$set_auto_save(auto_save)
      }
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
      usethis::ui_line( paste(
        crayon::bold("livecode server:"),
        crayon::red(private$file_cache$get_filename()),
        "@",
        crayon::underline(crayon::blue(self$url))
      ) )
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
    #' Start the server
    start = function() {
      private$server = file_stream_server(
        private$ip, private$port, private$file_cache,
        template = private$template, interval = private$interval
      )
      
      usethis::ui_done( paste(
        "Started sharing {usethis::ui_value(private$file_cache$get_filename())}",
        "at {usethis::ui_value(self$url)}."
      ) )
      
      if (is_ip_private(private$ip)) {
        usethis::ui_oops( paste(
          "The current ip address ({usethis::ui_value(private$ip)}) for the server is private,",
          "only users on the same local network are likely to be able to connect."
        ) )
      }
      
      register_server(self)
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
      
      deregister_server(self)
    },
    
    #' @description
    #' Restart the server
    restart = function() {
      if (self$is_running()) {
        self$stop()
      }
      
      private$start()
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
      private$file_cache$get_path()
    }
  )
)

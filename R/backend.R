
dynamic_site = function(
  dir = '.',
  ...,
  static_paths = NULL,
  build = function(...) FALSE,
  response = servr:::serve_dir(),
  ws_handler = ""
) {

  dir = normalizePath(dir, mustWork = TRUE)

  res = servr::server_config(dir, ...)


  withr::with_dir(dir, build(NULL))

  if (file.exists(ws_handler))
    ws_handler = readr::read_file(ws_handler)

  # FIXME
  js  = readr::read_file(system.file('resources','ws-create.html', package = "servr"))
  js = gsub('!!SERVR_HANDLER', ws_handler, js, fixed = TRUE)
  js = gsub('!!SERVR_INTERVAL', format(res$interval * 1000), js, fixed = TRUE)

  app = list(
    call = function(req) {
      withr::local_dir(dir)

      res = response(req)
      if (res$status != 200L || res$headers[['Content-Type']] != 'text/html')
        return(res)

      body = res$body
      if (is.raw(body))
        body = rawToChar(body)

      body = if (grepl('</head>', body)) {
        sub(
          '</head>', paste0(js, '\n</head>'), body,
          fixed = TRUE, useBytes = TRUE
        )
      } else if (!grepl('</html>', body)) {
        # there is no </head> or </html>, just append js after the document
        paste0(body, "\n", js)
      } else {
        usethis::ui_warn("Badly formated HTML in {usethis::ui_value(req$PATH_INFO)}.")
      }

      res$body = body
      res
    },

    onWSOpen = function(ws) {
      ws$onMessage(
        function(binary, message) {
          withr::local_dir(dir)

          ws$send(tryCatch(
            jsonlite::toJSON(
              build(jsonlite::fromJSON(message)),
              auto_unbox = TRUE, null = 'null'
            ),
            error = function(e) {
              print(e)
              'null'
            }
          ))
        }
      )
    }
  )

  app$staticPaths = static_paths

  res$start_server(app)
  invisible(res)
}

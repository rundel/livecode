#' Create a livecode server for broadcasting a file
#'
#' @param file Path to file to broadcast.
#' @param ip ip of the server, defaults to the top result of `network_interfaces`.
#' @param port port of the server, defaults to a random value.
#' @param interval page update interval in seconds.
#' @param bitly should a bitly bit link be created for the server.
#' @param auto_save should the broadcast file be auto saved during each update tic.
#' @param open_browser should a browser session be opened.
#'
#' @export

serve_file = function(file = NULL, ip = NULL, port = NULL, interval = 1,
                      bitly = FALSE, auto_save = TRUE, open_browser = TRUE) {
  
  server = livecode_server$new(file = file, ip = ip,
                               port = port, interval = interval,
                               bitly = bitly, auto_save = auto_save,
                               open_browser = open_browser)
  
  welcome_msg = c(
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
  
  #server$send_msg(text = welcome_msg)
  
  invisible(server)
}




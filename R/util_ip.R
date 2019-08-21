ipify_api_get_ipv4 = function() {
  res = httr::GET(
    "https://api.ipify.org"
  )

  httr::stop_for_status(res)
  httr::content(res, encoding = "UTF-8")
}

ipify_api_get_ipv6 = function() {
  res = httr::GET(
    "https://api6.ipify.org"
  )

  httr::stop_for_status(res)
  httr::content(res, encoding = "UTF-8")
}

#' @export
get_ip = function(type = c("ipv4", "ipv6")) {
  type = match.arg(type)
  switch(
    type,
    "ipv4" = ipify_api_get_ipv4(),
    "ipv6" = ipify_api_get_ipv6(),
    usethis::ui_stop("Unknown ip type {usethis::ui_value(type)}.")
  )
}

is_error = function(obj) {
  inherits(obj, "try-error")
}

port_available = function(host, port) {
  ll = httpuv:::logLevel("OFF")
  s = try({httpuv::startServer(host, port, list())}, silent = TRUE)
  httpuv:::logLevel(ll)

  if (is_error(s)) {
    FALSE
  } else {
    httpuv::stopServer(s)
    TRUE
  }
}

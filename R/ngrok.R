query_ngrok_public_url = function(api = "http://127.0.0.1:4040/api/tunnels",
                                  timeout = 5, interval = 0.2) {
  deadline = Sys.time() + timeout
  repeat {
    res = purrr::safely(httr::GET)(api, httr::timeout(1))
    if (succeeded(res)) {
      tunnels = jsonlite::fromJSON(
        httr::content(result(res), as = "text", encoding = "UTF-8"),
        simplifyVector = FALSE
      )[["tunnels"]]

      https = purrr::keep(tunnels, ~ identical(.[["proto"]], "https"))
      if (length(https) > 0)
        return(https[[1]][["public_url"]])

      if (length(tunnels) > 0)
        return(tunnels[[1]][["public_url"]])
    }

    if (Sys.time() >= deadline)
      return(NULL)

    Sys.sleep(interval)
  }
}

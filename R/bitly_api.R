bitly_api_host = "https://api-ssl.bitly.com/v4"

req_func = function(method) {
  method = toupper(method)
  switch(
    method,
    GET    = httr::GET,
    POST   = httr::POST,
    PATCH  = httr::PATCH,
    PUT    = httr::PUT,
    DELETE = httr::DELETE,
    usethis::ui_stop("Unknown http method: {usethis::ui_value(method)}.")
  )
}

bitly_api = function(endpoint, method = "GET", ..., token = bitly_get_token()) {
  req = req_func(method)(
    file.path(bitly_api_host, endpoint),
    httr::add_headers(
      Authorization = paste("Bearer", token)
    ),
    encode = "json",
    body = list(...)
  )

  res = httr::content(req)

  if (httr::status_code(req) != 200) {
    usethis::ui_stop( "{res[['description']]} ({res[['message']]})" )
  }

  res
}

bitly_api_user = function() {
  bitly_api("/user")
}

bitly_api_groups = function() {
  bitly_api("/groups")[["groups"]]
}

bitly_api_shorten = function(long_url, group_guid = bitly_get_groups()[1]) {
  bitly_api(
    "/shorten", "POST",
    group_guid = group_guid,
    domain = "bit.ly",
    long_url = URLencode(long_url)
  )
}

#' @export
bitly_get_groups = function() {
  l = bitly_api_groups()
  names = purrr::map_chr(l, "name")
  guids = purrr::map_chr(l, "guid")

  setNames(guids, names)
}

#' @export
bitly_shorten = function(url, guid = bitly_get_groups()[1]) {
  # TODO: add group handling
  link = bitly_api_shorten(url, guid)[["link"]]

  usethis::ui_done(
    "Created bitlink {usethis::ui_value(link)} for {usethis::ui_value(url)}."
  )

  link
}

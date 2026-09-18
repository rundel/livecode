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
  code = httr::status_code(req)

  if (code >= 300) {
    usethis::ui_stop("Bitly API error code ({code}) - {res[['message']]}")
  }

  res
}

bitly_api_user = function() {
  bitly_api("/user")
}

bitly_api_groups = function() {
  bitly_api("/groups")[["groups"]]
}

#' @importFrom utils URLencode browseURL
bitly_api_shorten = function(long_url, group_guid = bitly_get_groups()[1]) {
  bitly_api(
    "/shorten", "POST",
    group_guid = group_guid,
    domain = "bit.ly",
    long_url = URLencode(long_url)
  )
}

#' Retrieve available Bitly groups
#'
#' @description
#' Queries the authenticated Bitly account and returns the groups available
#' to the current user. Bitly groups are organizational containers used to
#' manage branded links, users, and link ownership.
#'
#' This function returns a named character vector where the names are group
#' names and the values are the corresponding Bitly group GUIDs.
#'
#' @return
#' A named character vector of Bitly group GUIDs. Names correspond to the
#' human-readable Bitly group names.
#'
#' @details
#' This function is primarily used internally by \code{bitly_shorten()} to
#' determine a default group when creating a short link.
#'
#' @examples
#' \dontrun{
#' # View available Bitly groups
#' bitly_get_groups()
#' }
#'
#' @importFrom stats setNames
#' @seealso
#' \code{\link{bitly_shorten}}
#'
#' @export
bitly_get_groups = function() {
  l = bitly_api_groups()
  names = purrr::map_chr(l, "name")
  guids = purrr::map_chr(l, "guid")
  
  setNames(guids, names)
}



#' Create a Bitly short link
#'
#' @description
#' Creates a shortened Bitly link for a supplied URL using the authenticated
#' Bitly account.
#'
#' By default, the short link is created using the first available Bitly group
#' returned by \code{bitly_get_groups()}.
#'
#' @param url Character string giving the URL to shorten.
#' @param guid Character string giving the Bitly group GUID under which the
#'   short link should be created. Defaults to the first available group.
#'
#' @return
#' A character string containing the shortened Bitly URL.
#'
#' @details
#' A success message is displayed when the short link is created.
#'
#' This function requires a valid Bitly personal access token configured via
#' \code{bitly_get_token()}.
#'
#' @examples
#' \dontrun{
#' # Shorten a local livecode URL
#' bitly_shorten("http://192.168.1.10:4321")
#'
#' # Shorten using a specific Bitly group
#' groups <- bitly_get_groups()
#' bitly_shorten("http://192.168.1.10:4321", guid = groups[1])
#' }
#'
#' @seealso
#' \code{\link{bitly_get_groups}}
#'
#' @export
bitly_shorten = function(url, guid = bitly_get_groups()[1]) {
  link = bitly_api_shorten(url, guid)[["link"]]
  
  usethis::ui_done(
    "Created bitlink {usethis::ui_value(link)} for {usethis::ui_value(url)}."
  )
  
  link
}

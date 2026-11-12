#' Keys for a tailcat address posted ahead of time
#'
#' @description
#' Creates a set of tailcat keys and its address, saved to \code{path}, or
#' reads the set saved there earlier. Post \code{keys$address} before class
#' and start the broadcast with \code{serve_file(file, tailcat = keys)} at
#' class time; the broadcast then has that address.
#'
#' @param path Path of the file holding the keys, created (readable by you
#'   only) on the first call and read on later calls. Name it after the
#'   class meeting, see Details.
#' @param timeout Seconds to wait for tailcat's relay when creating the keys.
#'
#' @details
#' Creating the keys starts a tailcat server for a moment, in a separate R
#' process, to learn the address, and stops it again. Nothing keeps running.
#'
#' The address is a credential: anyone who has it can follow every
#' broadcast started with these keys. Use a set of keys for one class
#' meeting only, since everyone who ever had the address keeps access while
#' the keys are used, and keep the file private. Post the address somewhere
#' only you can edit, such as the course site or an LMS announcement.
#'
#' An address names tailcat's relay region. Should the public relay map
#' change between creating the keys and the class, the broadcast gets a
#' different address and \code{serve_file()} says so.
#'
#' @return
#' A \code{livecode_tailcat_keys} object, a list with \code{address},
#' \code{public_key}, the secret \code{private_key} and
#' \code{preshared_key}, and \code{created}. Printing it never shows the
#' secrets or the address.
#'
#' @examples
#' \dontrun{
#' keys = tailcat_keys("stat101-2026-11-12.rds")
#' keys$address # post this
#'
#' # At class time
#' srv = serve_file("lecture.R", tailcat = keys)
#' }
#'
#' @seealso \code{\link{serve_file}}, \code{\link{follow}}
#'
#' @export
tailcat_keys = function(path, timeout = 60) {
  if (!is_string(path) || !nzchar(path))
    cli::cli_abort("{.arg path} must be a single file path.")
  path = path.expand(path)

  if (file.exists(path))
    return(tc_read_keys(path))

  tc_check_installed(c("rtailcat", "callr"), "Creating tailcat keys")
  if (!dir.exists(dirname(path)))
    cli::cli_abort("The directory {.file {dirname(path)}} does not exist.")

  tryCatch(
    callr::r(
      function(path, timeout) {
        Sys.umask("077")
        keys = rtailcat::tailcat_keygen()
        srv = rtailcat::TailcatServer$new(key = keys)
        on.exit(srv$close(), add = TRUE)
        srv$start(timeout = timeout)
        out = list(
          address = as.character(unclass(srv$address)),
          public_key = as.character(keys$public_key),
          private_key = as.character(unclass(keys$private_key)),
          preshared_key = as.character(unclass(keys$preshared_key)),
          created = Sys.time()
        )
        saveRDS(structure(out, class = "livecode_tailcat_keys"), path)
        Sys.chmod(path, "0600", use_umask = FALSE)
        invisible(TRUE)
      },
      args = list(path = path, timeout = timeout)
    ),
    error = function(e) {
      cli::cli_abort("Could not create tailcat keys.", parent = e)
    }
  )

  tc_read_keys(path)
}

tc_read_keys = function(path) {
  keys = tryCatch(readRDS(path), error = function(e) NULL)
  fields = c("address", "public_key", "private_key", "preshared_key")
  ok = inherits(keys, "livecode_tailcat_keys") &&
    all(vapply(fields, function(f) is_string(keys[[f]]) && nzchar(keys[[f]]), logical(1)))
  if (!ok) {
    cli::cli_abort(c(
      "{.file {path}} does not hold livecode tailcat keys.",
      "i" = "Pass the path of a new file to create keys."
    ))
  }
  attr(keys, "path") = path
  keys
}

#' @export
format.livecode_tailcat_keys = function(x, ...) {
  created = x[["created"]]
  c(
    "<livecode tailcat keys>",
    if (!is.null(attr(x, "path"))) paste0("  file:          ", attr(x, "path")),
    if (!is.null(created)) paste0("  created:       ", format(created, "%Y-%m-%d %H:%M")),
    paste0("  public_key:    ", x[["public_key"]]),
    "  address:       <redacted>, see $address",
    "  private_key:   <redacted>",
    "  preshared_key: <redacted>"
  )
}

#' @export
print.livecode_tailcat_keys = function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @exportS3Method utils::str
str.livecode_tailcat_keys = function(object, ...) {
  cat("<livecode tailcat keys> (secrets redacted)\n")
  invisible(object)
}

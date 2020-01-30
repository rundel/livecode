#' Server for broadcasting source code to multiple viewers
#'
#' Broadcast a local R (or other text) document over the web and provide live updates as it is edited.
#'
#' @seealso \link{serve_file}
#'
#' @name livecode-package
#' @aliases livecode
#' @docType package
#' @title Source code broadcasting server
#' @author Colin Rundel \email{rundel@gmail.com}
#' @keywords package
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib livecode, .registration = TRUE
## usethis namespace: end
NULL

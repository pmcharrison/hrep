# Properties ####
#' @export
num_sequences <- function(x) UseMethod("num_sequences")

#' Number of symbols
#'
#' Number of symbols in an object (includes duplicates)
#' @export
num_symbols <- function(x) UseMethod("num_symbols")

#' #' @export
#' get_transpositions <- function(x) UseMethod("get_transpositions")
#' #' @export
#' normalise_bass <- function(x) UseMethod("normalise_bass")

#' @export
metadata <- function(x) UseMethod("metadata")
#' @export
`metadata<-` <- function(x, value) UseMethod("metadata<-")

#' @export
type <- function(x) UseMethod("type")
#' @export
`type<-` <- function(x, value) UseMethod("type<-")

#' @export
transform_symbols <- function(x, f) UseMethod("transform_symbols")

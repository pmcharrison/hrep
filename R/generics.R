# Properties ####
#' @export
num_sequences <- function(x) UseMethod("num_sequences")

#' Number of symbols
#'
#' Number of symbols in an object (includes duplicates)
#' @export
num_symbols <- function(x) UseMethod("num_symbols")

#' @export
get_transpositions <- function(x) UseMethod("get_transpositions")

#' @export
normalise_bass <- function(x) UseMethod("normalise_bass")

#' @export
metadata <- function(x) UseMethod("metadata")
#' @export
metadata.default <- function(x) list()
#' @export
`metadata<-` <- function(x, value) UseMethod("metadata<-")

#' @export
type <- function(x) UseMethod("type")

#' @export
type.default <- function(x) stop("x had undefined type")

#' @export
`type<-` <- function(x, value) UseMethod("type<-")

#' @export
transform_symbols <- function(x, f, type) UseMethod("transform_symbols")

#' @export
is.empty <- function(x) UseMethod("is.empty")

#' @export
is.coded <- function(x) UseMethod("is.coded")

#' @export
x_unit <- function(x) UseMethod("x_unit")

#' @export
y_unit <- function(x) UseMethod("y_unit")

#' @export
`y_unit<-` <- function(x, value) UseMethod("y_unit<-")

#' @export
lower <- function(x) UseMethod("lower")

#' @export
upper <- function(x) UseMethod("upper")

#' @export
low_eq <- function(x) UseMethod("low_eq")

#' @export
high_eq <- function(x) UseMethod("high_eq")

#' @export
label <- function(x) UseMethod("label")

#' @export
x_lab <- function(x) UseMethod("x_lab")

#' @export
y_lab <- function(x) UseMethod("y_lab")

#' @export
`y_lab<-` <- function(x, value) UseMethod("y_lab<-")

#' @export
view <- function(x, ...) UseMethod("view")

#' @export
transform_y <- function(x, f, y_unit, y_lab) UseMethod("transform_y")

#' @export
freq <- function(x) UseMethod("freq")

#' @export
`freq<-` <- function(x, value) UseMethod("freq<-")

#' @export
pitch <- function(x) UseMethod("pitch")

#' @export
`pitch<-` <- function(x, value) UseMethod("pitch<-")

#' @export
amp <- function(x) UseMethod("amp")

#' @export
`amp<-` <- function(x, value) UseMethod("amp<-")

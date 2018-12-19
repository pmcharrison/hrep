#' Number of sequences
#'
#' Counts the number of sequences in an object.
#' @param x Object to analyse.
#' @return The number of sequences in \code{x}, as an integer scalar.
#' @rdname num_sequences
#' @export
num_sequences <- function(x) UseMethod("num_sequences")

#' Size
#'
#' Returns the size of an object, counted in symbols.
#' For an object of class \code{\link{{vec}}, this will be the length of the vector.
#' For an object of class \code{\link{{corpus}},
#' this will be the sum of the lengths of its constituent sequences.
#' @param x Object to analyse.
#' @return The size of \code{x}, as an integer scalar.
#' @rdname size
#' @export
size <- function(x) UseMethod("size")

#' Metadata
#'
#' Additional information about an object may be stored in its
#' metadata attribute.
#' @param x Method whose metadata should be accessed.
#' @rdname metadata
#' @export
metadata <- function(x) UseMethod("metadata")

#' @rdname metadata
#' @export
metadata.default <- function(x) list()

#' @rdname metadata
#' @export
`metadata<-` <- function(x, value) UseMethod("metadata<-")

#' Type
#'
#' A \strong{type} is a way of representing a chord symbol.
#'
#' @details
#' Various types are available:
#' * \code{\link{fr_chord}}
#' * \code{\link{fr_sparse_spectrum}}
#' * \code{\link{pi_chord}}
#' * \code{\link{pi_sparse_spectrum}}
#' * \code{\link{pc_chord}}
#' * \code{\link{pc_chord_type}}
#' * \code{\link{pc_set}}
#' * \code{\link{pc_set_norm_form}}
#' * \code{\link{pc_set_norm_order}}
#' * \code{\link{pc_milne_spectrum}}
#'
#' Various objects can be typed:
#' * Individual chord objects, e.g. \code{\link{pc_set}}
#' * Chord sequences, as created by \code{\link{vec}} and \code{\link{coded_vec}}
#' * Corpora of chord sequences, as created by \code{\link{corpus}}
#' @param x Object whose type should be accessed.
#' @return The type of \code{x}.
#' @md
#' @rdname type
#' @export
type <- function(x) UseMethod("type")

#' @export
type.default <- function(x) stop("x had undefined type")

`type<-` <- function(x, value) UseMethod("type<-")

#' Transform symbols
#'
#' Transforms the symbols within an object.
#' @param x Object to transform.
#' @param f Function to apply to each symbol.
#' @param type Character scalar identifying the new symbol type.
#' @param ... Further arguments to be passed to methods.
#' @rdname transform_symbols
#' @export
transform_symbols <- function(x, f, type, ...) {
  UseMethod("transform_symbols")
}

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

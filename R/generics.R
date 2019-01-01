#' Number of sequences
#'
#' Counts the number of sequences in an object.
#' @param x Object to analyse.
#' @return The number of sequences in \code{x}, as an integer scalar.
#' @rdname num_sequences
#' @export
num_sequences <- function(x) UseMethod("num_sequences")

#' Number of elements
#'
#' Returns the number of elements in an object.
#' For an object of class \code{\link{vec}}, this will be the length of the vector.
#' For an object of class \code{\link{corpus}},
#' this will be the sum of the lengths of its constituent sequences.
#' @param x Object to analyse.
#' @return The size of \code{x}, as an integer scalar.
#' @rdname num_elements
#' @export
num_elements <- function(x) UseMethod("num_elements")

#' Metadata
#'
#' Additional information about an object may be stored in its
#' metadata attribute.
#' @param x Method whose metadata should be accessed.
#' @rdname metadata
#' @export
metadata <- function(x) UseMethod("metadata")

#' @export
metadata.default <- function(x) list()

#' @param value New value.
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
#' * \code{\link{sparse_fr_spectrum}}
#' * \code{\link{pi_chord}}
#' * \code{\link{sparse_pi_spectrum}}
#' * \code{\link{pc_chord}}
#' * \code{\link{pc_chord_type}}
#' * \code{\link{pc_set}}
#' * \code{\link{pc_set_norm_form}}
#' * \code{\link{pc_set_norm_order}}
#' * \code{\link{milne_pc_spectrum}}
#'
#' Various objects can be typed:
#' * Individual chord objects, e.g. \code{\link{pc_set}}
#' * Chord sequences, as created by \code{\link{vec}} and \code{\link{coded_vec}}
#' * Corpora of chord sequences, as created by \code{\link{corpus}}
#' @param x Object whose type should be accessed.
#' @return The type of \code{x}.
#' @md
#' @export
type <- function(x) {
  UseMethod("type")
}

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
#' @return The transformed object.
#' @export
transform_symbols <- function(x, f, type, ...) {
  UseMethod("transform_symbols")
}

#' Is an object coded?
#'
#' Checks whether an object is integer-coded.
#' @param x Object to check.
#' @return Scalar logical.
#' @seealso \code{\link{encode}}, \code{\link{decode}}
#' @export
is.coded <- function(x) UseMethod("is.coded")

#' View
#'
#' Views an object.
#' @param x Object to view.
#' @param ... Parameters passed to methods.
#' @rdname view
#' @export
view <- function(x, ...) UseMethod("view")

#' Transform y values
#'
#' Transforms an object's y values.
#' @param x Object to transform.
#' @param f Function to apply (vectorised).
#' @param y_unit (Character scalar) New units for the y axis.
#' @param y_lab (Character scalar) New label for the y axis.
#' @export
transform_y <- function(x, f, y_unit, y_lab) UseMethod("transform_y")

.representations <- c(
  "fr_chord",
  "milne_pc_spectrum",
  "pc_chord",
  "pc_chord_type",
  "pc_set",
  "pc_set_norm_order",
  "pc_set_type",
  "pi_chord",
  "pi_chord_type",
  "smooth_pc_spectrum",
  "smooth_pi_spectrum",
  "sparse_fr_spectrum",
  "sparse_fr_spectrum",
  "sparse_pc_spectrum",
  "sparse_pi_spectrum",
  "wave"
)

#' Representations
#'
#' Returns the available representations in the hrep package.
#' @return Character vector of representation labels.
representations <- function() .representations

#' Represent
#'
#' Represents an object using a particular type.
#' @param x Input object.
#' @param type Target type (see \code{\link{type}}).
#' @param ... Further arguments to pass to \code{\link{represent}()}.
#' @rdname represent
#' @export
represent <- function(x, type, ...) {
  UseMethod("represent")
}

#' @rdname represent
#' @export
represent.default <- function(x, type, ...) {
  checkmate::qassert(type, "S1")
  stopifnot(type %in% representations())
  get(type, mode = "function")(x, ...)
}

#' @rdname represent
#' @export
represent.corpus <- function(x, type, ...) {
  f <- function(sym, ...) represent(sym, type, ...)
  transform_symbols(x, f, type, ...)
}

#' @rdname represent
#' @export
represent.vec <- function(x, type, ...) {
  f <- function(sym, ...) represent(sym, type, ...)
  transform_symbols(x, f, type, ...)
}

#' @rdname represent
#' @export
represent.coded_vec <- function(x, type, ...) {
  f <- function(sym, ...) represent(sym, type, ...)
  transform_symbols(x, f, type, ...)
}

#' @rdname represent
#' @export
represent.list <- function(x, type, ...) {
  f <- function(sym, ...) represent(sym, type, ...)
  vec(
    x = purrr::map(x, f, ...),
    type = type
  )
}

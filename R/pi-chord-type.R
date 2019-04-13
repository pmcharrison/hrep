#' Pitch chord type constructor
#'
#' This is the low-level construct for pitch chord type.
#' @keywords internal
.pi_chord_type <- function(x) {
  checkmate::qassert(x, "N+")
  x <- as.numeric(x)
  stopifnot(x[1] == 0,
            !anyDuplicated(x),
            isTRUE(all.equal(x, sort(x))))
  class(x) <- c("pi_chord_type", "pi_chord", "chord")
  x
}

#' Pitch chord type
#'
#' This function represents an object as a pitch chord type.
#' A pitch chord type is defined as the chord's pitches
#' expressed relative to the bass pitch.
#' @param x Object to represent as a pitch chord type.
#' @return Returns an object of class \code{pi_chord_type}.
#' @rdname pi_chord_type
#' @export
pi_chord_type <- function(x) {
  UseMethod("pi_chord_type")
}

#' @export
#' @rdname pi_chord_type
pi_chord_type.default <- function(x) {
  pi_chord_type(pi_chord(x))
}

#' @export
#' @rdname pc_chord_type
pi_chord_type.pi_chord <- function(x) {
  .pi_chord_type(tp(x, - get_bass_pi(x)))
}

#' @export
#' @rdname pc_chord_type
pi_chord_type.smooth_spectrum <- function(x) {
  stop("no methods exist for coercing smooth spectra to pitch-class chord types")
}

#' @export
#' @rdname pc_chord_type
pi_chord_type.sparse_spectrum <- function(x) {
  stop("no methods exist for coercing sparse spectra to pitch-class chord types")
}

#' @export
print.pi_chord_type <- function(x, ...) {
  cat("Pitch chord type: ",
      as.character(x),
      sep = "")
}

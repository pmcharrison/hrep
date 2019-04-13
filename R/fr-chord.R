#' Frequency chord constructor
#'
#' This hidden function constructs a frequency chord object.
#' It is unforgiving with respect to input formats,
#' unlike \code{\link{fr_chord}}.
#' @param x (Numeric vector) Frequencies in ascending order
#' @keywords internal
.fr_chord <- function(...) {
  x <- unclass(c(...))
  checkmate::qassert(x, "N+")
  stopifnot(!anyDuplicated(x), isTRUE(all.equal(x, sort(x))))
  class(x) <- c("fr_chord", "chord", "numeric")
  x
}

#' Frequency chord
#'
#' This function represents an object as a frequency chord.
#' A frequency chord is defined as a set of non-duplicated
#' frequencies, expressed in Hz.
#' @param x Object to represent as a frequency chord.
#' @return Returns an object of class \code{fr_chord}.
#' @export
#' @rdname fr_chord
#' @export
fr_chord <- function(x) {
  UseMethod("fr_chord")
}

#' @export
#' @rdname fr_chord
fr_chord.numeric <- function(x) {
  .fr_chord(sort(unique(unclass(x))))
}

#' @export
#' @rdname fr_chord
fr_chord.character <- function(x) {
  stopifnot(length(x) == 1L)
  y <- as.numeric(strsplit(x, split = " ")[[1]])
  if (anyNA(y)) stop("malformed character input, should be of the form ",
                     "'330.25 440 457.90'")
  fr_chord(y)
}

#' @export
as.character.fr_chord <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

#' @export
as.numeric.fr_chord <- function(x, ...) {
  unclass(x)
}

#' @export
#' @rdname fr_chord
fr_chord.pi_chord <- function(x) {
  .fr_chord(midi_to_freq(as.numeric(x)))
}

#' @export
#' @rdname fr_chord
fr_chord.pc_set <- function(x) {
  fr_chord(pi_chord(x))
}

#' @export
#' @rdname fr_chord
fr_chord.pc_chord <- function(x) {
  fr_chord(pi_chord(x))
}

#' @export
#' @rdname fr_chord
fr_chord.fr_chord <- function(x) {
  x
}

#' @rdname fr_chord
#' @export
is.fr_chord <- function(x) is(x, "fr_chord")

#' @export
print.fr_chord <- function(x, digits = 3L, ...) {
  cat("Frequency chord: ",
      paste(paste(round(as.numeric(x), digits = digits), "Hz"),
            collapse = ", "), "\n", sep = "")
}

#' @export
c.fr_chord <- function(...) {
  x <- lapply(list(...), unclass)
  x <- do.call(c, x)
  fr_chord(sort(unique(x)))
}

#' Get bass frequency
#'
#' Gets the bass frequency from a sonority.
#' @param x Object to analyse.
#' @return Bass frequency, as a numeric scalar.
#' @rdname get_bass_fr
#' @export
get_bass_fr <- function(x) {
  UseMethod("get_bass_fr")
}

#' @rdname get_bass_fr
#' @export
get_bass_fr.default <- function(x) {
  get_bass_fr(fr_chord(x))
}

#' @rdname get_bass_fr
#' @export
get_bass_fr.fr_chord <- {
  function(x) x[1]
}

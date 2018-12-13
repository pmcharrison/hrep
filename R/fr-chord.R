#' @param x (Numeric vector) Frequencies in ascending order
#' @export
.fr_chord <- function(...) {
  x <- unclass(c(...))
  checkmate::qassert(x, "N+")
  stopifnot(!anyDuplicated(x), isTRUE(all.equal(x, sort(x))))
  class(x) <- c("fr_chord", "numeric")
  x
}

#' @export
fr_chord <- function(x) {
  UseMethod("fr_chord")
}

#' @export
fr_chord.numeric <- function(x) {
  .fr_chord(sort(unique(unclass(x))))
}

#' @export
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
fr_chord.pi_chord <- function(x) {
  .fr_chord(midi_to_freq(as.numeric(x)))
}

#' @export
fr_chord.pc_set <- function(x) {
  fr_chord(pi_chord(x))
}

#' @export
fr_chord.pc_chord <- function(x) {
  fr_chord(pi_chord(x))
}

#' @export
fr_chord.fr_chord <- function(x) {
  x
}

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

#' @export
get_bass_fr <- function(x, ...) {
  UseMethod("get_bass_fr")
}

#' @export
get_bass_fr.fr_chord <- {
  function(x, ...) x[1]
}

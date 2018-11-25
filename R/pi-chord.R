#' @param x (Numeric vector) MIDI note numbers in ascending order
#' @export
.pi_chord <- function(...) {
  x <- unclass(c(...))
  checkmate::qassert(x, "N")
  stopifnot(!anyDuplicated(x), isTRUE(all.equal(x, sort(x))))
            class(x) <- c("pi_chord", "numeric")
            x
}

#' @export
pi_chord <- function(x) {
  UseMethod("pi_chord")
}

#' @export
pi_chord.numeric <- function(x) {
  .pi_chord(sort(unique(unclass(x))))
}

#' @export
pi_chord.pc_set <- function(x) {
  ref <- if (is.integer(x)) 60L else 60
  pi_chord(ref + x)
}

#' @export
pi_chord.pc_chord <- function(x) {
  .pi_chord(c(48 + get_bass_pc(x),
              60 + get_non_bass_pc(x)))
}

#' @export
pi_chord.pi_chord <- function(x) {
  x
}

#' @export
is.pi_chord <- function(x) is(x, "pi_chord")

#' @export
print.pi_chord <- function(x, ...) {
  cat("Pitch chord: ", paste(x, collapse = " "), "\n", sep = "")
}

#' @export
c.pi_chord <- function(...) {
  x <- lapply(list(...), unclass)
  x <- do.call(c, x)
  pi_chord(sort(unique(x)))
}

#' @export
view.pi_chord <- function(x, ...) {
  abcR::view_pi_chord(x, ...)
}

#' @export
get_bass_pi <- function(x, ...) {
  UseMethod("get_bass_pi")
}

#' @export
get_bass_pi.pi_chord <- {
  function(x, ...) x[1]
}

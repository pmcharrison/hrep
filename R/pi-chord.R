#' pi_chord constructor
#'
#' Constructor function for objects of class "pi_chord".
#' @param x (Numeric vector) MIDI note numbers in ascending order
#' @keywords internal
.pi_chord <- function(...) {
  x <- unclass(c(...))
  checkmate::qassert(x, "N+")
  stopifnot(!anyDuplicated(x), isTRUE(all.equal(x, sort(x))))
            class(x) <- c("pi_chord", "numeric")
            x
}

#' Pitch chord
#'
#' This function represents an object as a pitch chord.
#' A pitch chord is defined as a set of non-duplicated
#' pitches, expressed as MIDI note numbers.
#' @param x Object to represent as a pitch chord.
#' @return Returns an object of class \code{pi_chord}.
#' @export
#' @rdname pi_chord
#' @export
pi_chord <- function(x) {
  UseMethod("pi_chord")
}

#' @export
#' @rdname pi_chord
pi_chord.numeric <- function(x) {
  .pi_chord(sort(unique(unclass(x))))
}

#' @export
#' @rdname pi_chord
pi_chord.character <- function(x) {
  stopifnot(length(x) == 1L)
  y <- as.numeric(strsplit(x, split = " ")[[1]])
  if (anyNA(y)) stop("malformed character input, should be of the form ",
                     "'60 64 67'")
  pi_chord(y)
}

#' @export
as.numeric.pi_chord <- function(x, ...) {
  unclass(x)
}

#' @export
as.character.pi_chord <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

#' @export
#' @rdname pi_chord
pi_chord.pc_set <- function(x) {
  ref <- if (is.integer(x)) 60L else 60
  pi_chord(ref + x)
}

#' @export
#' @rdname pi_chord
pi_chord.pc_chord <- function(x) {
  .pi_chord(c(48 + get_bass_pc(x),
              60 + get_non_bass_pc(x)))
}

#' @export
#' @rdname pi_chord
pi_chord.pi_chord <- function(x) {
  x
}

#' @export
#' @rdname pi_chord
pi_chord.fr_chord <- function(x) {
  pi_chord(freq_to_midi(as.numeric(x)))
}

#' @export
#' @rdname pi_chord
pi_chord.pi_chord_type <- function(x) {
  .pi_chord(60 + x)
}

#' Check for type "pi_chord"
#'
#' Checks whether an object is of type "pi_chord".
#' @param x Object to check.
#' @return Logical scalar.
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

#' @rdname view
#' @export
view.pi_chord <- function(x, ...) {
  abcR::view_pi_chord(x, ...)
}

#' Get bass pitch
#'
#' Gets the bass pitch of a sonority.
#' @param x Input sonority.
#' @return The bass pitch, as a MIDI note number (numeric scalar).
#' @rdname get_bass_pi
#' @export
get_bass_pi <- function(x) {
  UseMethod("get_bass_pi")
}

#' @rdname get_bass_pi
#' @export
get_bass_pi.default <- function(x) {
  get_bass_pi(pi_chord(x))
}

#' @rdname get_bass_pi
#' @export
get_bass_pi.pi_chord <- function(x) x[1]

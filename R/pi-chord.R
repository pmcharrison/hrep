#' @param x (Numeric vector) MIDI note numbers
#' @export
pi_chord <- function(x) {
  checkmate::qassert(x, "N")
  x <- sort(unique(x))
  class(x) <- c("pi_chord", "numeric")
  x
}

#' @export
print.pi_chord <- function(x, ...) {
  cat("Pitch chord: ", paste(x, collapse = " "), "\n", sep = "")
}

#' @export
as.pi_chord <- function(x) UseMethod("as.pi_chord")

#' @export
as.pi_chord.numeric <- function(x) {
  pi_chord(x)
}

#' @export
get_bass_pi <- function(x, ...) UseMethod("get_bass_pi")

#' @export
get_bass_pi.pi_chord <- function(x, ...) x[1]

#' @export
as.pc_chord.pi_chord <- function(x) {
  x <- as.numeric(x)
  pc_chord(bass_pc = pi_to_pc(x[1]), other_pc = pi_to_pc(x[-1])
}

#' @export
as.pc_set.pi_chord <- function(x) {
  x <- as.numeric(x)
  pc_set(sort(unique(pi_to_pc(x))))
}

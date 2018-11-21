#' @param x (Numeric vector) MIDI note numbers
#' @export
pi_chord <- function(x, safe = TRUE) {
  checkmate::qassert(x, "N")
  if (safe) x <- sort(unique(x))
  class(x) <- c("pi_chord", "numeric")
  x
}

#' @export
print.pi_chord <- function(x, ...) {
  cat("Pitch chord: ", paste(x, collapse = " "), "\n", sep = "")
}

#' @export
as.pi_chord <- function(x, safe = TRUE) UseMethod("as.pi_chord")

#' @export
as.pi_chord.numeric <- function(x, safe = TRUE) {
  pi_chord(x, safe = safe)
}

#' @export
get_bass_pi <- function(x, ...) UseMethod("get_bass_pi")

#' @export
get_bass_pi.pi_chord <- function(x, ...) x[1]

#' @export
as.pc_chord.pi_chord <- function(x, safe = TRUE) {
  pc_chord(bass_pc = pi_to_pc(get_bass_pi(x)),

}

#' @export
normalise_bass.pi_chord <- function(x) {
  tp(x, - get_bass_pi(x))
}

#' @export
normalise_bass.pc_chord <- function(x) {
  tp(x, - get_bass_pc(x))
}

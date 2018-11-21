#' @param bass_pc Numeric scalar corresponding to bass pitch class
#' @param pc_set Numeric vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
pc_chord <- function(bass_pc, other_pc) {
  checkmate::qassert(bass_pc, "N1[0,12)")
  checkmate::qassert(other_pc, "N[0,12)")
  other_pc <- setdiff(sort(unique(other_pc)), bass_pc)
  x <- c(bass_pc, other_pc)
  class(x) <- "pc_chord"
  x
}

#' @export
print.pc_chord <- function(x, ...) {
  cat("Pitch-class chord: ",
      "[", get_bass_pc(x), "] ",
      paste(get_non_bass_pc(x), collapse = " "), "\n",
      sep = "")
}

#' @export
as.pc_chord <- function(x) UseMethod("as.pc_chord")
#' @export
as.pc_chord.numeric <- function(x) {
  pc_chord(bass_pc = x[1], other_pc = x[-1])
}
#' @export
as.pc_chord.pc_chord <- function(x) {
  x
}

#' @export
get_bass_pc <- function(x) UseMethod("get_bass_pc")
#' @export
get_bass_pc.pc_chord <- function(x) x[1]

#' @export
get_non_bass_pc <- function(x) UseMethod("get_non_bass_pc")
#' @export
get_non_bass_pc.pc_chord <- function(x) pc_set(x[- 1])

#' @export
as.numeric.pc_chord <- function(x) {
  class(x) <- "numeric"
  x
}

#' @export
as.integer.pc_chord <- function(x) {
  as.integer(as.numeric(x))
}

#' @export
as.pc_set.pc_chord <- function(x) {
  pc_set(sort(unique(as.numeric(x))))
}

#' @export
normalise_bass.pc_chord <- function(x) {
  transpose(x, - get_bass_pc(x))
}

#' @export
get_transpositions.pc_chord <- function(x) {
  ref <- normalise_bass(x)
  lapply(0:11, function(int) transpose(ref, int))
}

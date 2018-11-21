#' @export
pc_set <- function(pitch_classes) {
  checkmate::qassert(pitch_classes, "X[0,12)")
  stopifnot(!anyDuplicated(pitch_classes))
  pitch_classes <- sort(as.integer(pitch_classes))

  class(pitch_classes) <- "pc_set"
  pitch_classes
}

#' @export
as.pc_set <- function(x) UseMethod("as.pc_set")
#' @export
as.pc_set.pc_set <- function(x) {
  pc_set(as.integer(x))
}
#' @export
as.pc_set.pc_chord <- function(x) {
  get_pc_set.pc_chord(x)
}
#' @export
as.pc_set.numeric <- function(x, ...) {
  pc_set(x)
}

#' @export
print.pc_set <- function(x, ...) {
  cat("Pitch-class set: ",
      paste0("[", paste(x, collapse = ", "), "]\n"),
      sep = "")
}

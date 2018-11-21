#' @export
pc_set <- function(pitch_classes, safe = TRUE) {
  if (safe) {
    checkmate::qassert(pitch_classes, "X[0,12)")
    stopifnot(!anyDuplicated(pitch_classes))
    pitch_classes <- sort(as.integer(pitch_classes))
  }
  class(pitch_classes) <- "pc_set"
  pitch_classes
}

#' @export
as.pc_set <- function(x, safe = TRUE) UseMethod("as.pc_set")
#' @export
as.pc_set.pc_set <- function(x, safe = TRUE) {
  if (safe) {
    x <- pc_set(as.integer(x), safe = TRUE)
  }
  x
}
#' @export
as.pc_set.pc_chord <- function(x, safe = TRUE) {
  get_pc_set.pc_chord(x, safe = safe)
}
#' @export
as.pc_set.numeric <- function(x, safe = TRUE, ...) {
  pc_set(x, safe = safe)
}

#' @export
print.pc_set <- function(x, ...) {
  cat("Pitch-class set: ",
      paste0("[", paste(x, collapse = ", "), "]\n"),
      sep = "")
}

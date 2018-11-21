#' @export
pc_set <- function(pitch_classes) {
  checkmate::qassert(pitch_classes, "X[0,12)")
  pitch_classes <- sort(unique(pitch_classes))
  class(pitch_classes) <- "pc_set"
  pitch_classes
}

#' @export
as.pc_set <- function(x) UseMethod("as.pc_set")
#' @export
as.pc_set.pc_set <- function(x) x
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

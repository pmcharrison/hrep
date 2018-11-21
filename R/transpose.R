# pc_set ####

#' @export
transpose <- function(x, interval) UseMethod("transpose")

#' @export
transpose.pc_set <- function(x, interval) {
  checkmate::qassert(interval("N1"))
  pc_set(sort((as.numeric(x) + interval) %% 12L))
}

#' @export
transpose.pc_chord <- function(x, interval) {
  checkmate::qassert(interval("N1"))
  x <- (x + interval) %% 12L
  if (length(x) > 1) {
    x[- 1] <- sort(x[- 1])
  }
  x
}

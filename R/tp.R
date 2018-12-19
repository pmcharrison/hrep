#' Transpose
#'
#' Transposes a sonority.
#' @param x Input sonority.
#' @param interval (Numeric scalar) Transposition interval.
#' @return Transposed sonority.
#' @rdname tp
#' @export
tp <- function(x, interval) UseMethod("tp")

#' @rdname tp
#' @export
tp.pc_set <- function(x, interval) {
  checkmate::qassert(interval, "N1")
  pc_set(sort((as.numeric(x) + interval) %% 12L))
}

#' @rdname tp
#' @export
tp.pc_chord <- function(x, interval) {
  checkmate::qassert(interval, "N1")
  x <- as.numeric(x)
  x <- (x + interval) %% 12L
  if (length(x) > 1) {
    x[- 1] <- sort(x[- 1])
  }
  pc_chord(x)
}

#' @rdname tp
#' @export
tp.pi_chord <- function(x, interval) {
  checkmate::qassert(interval, "N1")
  x <- as.numeric(x)
  .pi_chord(x + interval)
}

#' @export
Ops.pc_set <- function(e1, e2) {
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  NextMethod()
}

#' @export
Ops.pc_chord <- function(e1, e2) {
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  NextMethod()
}

#' @export
Ops.pi_chord <- function(e1, e2) {
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  NextMethod()
}

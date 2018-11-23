#' @export
transpose <- function(x, interval) UseMethod("transpose")

#' @export
transpose.pc_set <- function(x, interval) {
  checkmate::qassert(interval, "N1")
  pc_set(sort((as.numeric(x) + interval) %% 12L))
}

#' @export
transpose.pc_chord <- function(x, interval) {
  checkmate::qassert(interval, "N1")
  x <- as.numeric(x)
  x <- (x + interval) %% 12L
  if (length(x) > 1) {
    x[- 1] <- sort(x[- 1])
  }
  pc_chord(x[1], x[- 1])
}

#' @export
Ops.pc_set <- function(e1, e2) {
  stop("cannot perform arithmetic operations on pitch-class sets, ",
       "call 'as.numeric()' first")
}
#' @export
Ops.pc_chord <- function(e1, e2) {
  stop("cannot perform arithmetic operations on pitch-class chords, ",
       "call 'as.numeric()' first")
}
#' @export
Ops.pi_chord <- function(e1, e2) {
  stop("cannot perform arithmetic operations on pitch chords, ",
       "call 'as.numeric()' first")
}

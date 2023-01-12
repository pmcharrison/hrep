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

#' @rdname tp
#' @export
tp.corpus <- function(x, interval) {
  purrr::map(x, tp, interval = interval) %>%
    corpus(type = type(x), metadata = metadata(x))
}

#' @rdname tp
#' @export
tp.vec <- function(x, interval) {
  purrr::map(x, tp, interval = interval) %>%
    vec(type = type(x))
}

#' @rdname tp
#' @export
tp.coded_vec <- function(x, interval) {
  decode(x) %>%
    purrr::map(tp, interval = interval) %>%
    vec(type = type(x)) %>%
    encode()
}

#' @rdname tp
#' @export
tp.coded_vec_pc_set <- function(x, interval) {
  checkmate::qassert(interval, "X1")
  interval <- interval %% 12

  if (interval == 0) {
    x
  } else {
    pc_set_transpositions[[interval]][x] %>%
      coded_vec(type = type(x), metadata = metadata(x))

  }
}

#' @rdname tp
#' @export
tp.coded_vec_pc_chord <- function(x, interval) {
  checkmate::qassert(interval, "X1")
  interval <- interval %% 12

  if (interval == 0) {
    x
  } else {
    pc_chord_transpositions[[interval]][x] %>%
      coded_vec(type = type(x), metadata = metadata(x))
  }
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

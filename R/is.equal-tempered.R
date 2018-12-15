#' Check for equal temperament
#'
#' Checks whether a chord is equal-tempered.
#' @param x Chord to check.
#' @param ... Further arguments passed to other methods.
#' @return Returns \code{TRUE} if the chord is equal-tempered,
#' \code{FALSE} otherwise.
#' @rdname is.equal_tempered
#' @export
is.equal_tempered <- function(x, ...) {
  UseMethod("is.equal_tempered")
}

#' @rdname is.equal_tempered
#' @export
is.equal_tempered.pi_chord <- function(x, ...) {
  checkmate::qtest(x, "X")
}

#' @rdname is.equal_tempered
#' @export
is.equal_tempered.pc_set <- function(x, ...) {
  checkmate::qtest(x, "X")
}

#' @rdname is.equal_tempered
#' @export
is.equal_tempered.pc_chord <- function(x, ...) {
  checkmate::qtest(x, "X")
}

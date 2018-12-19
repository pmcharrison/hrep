.int_vec <- function(x) {
  x <- as.integer(x)
  checkmate::qassert(x, "I6[0,)")
  class(x) <- c("int_vec", "integer")
  x
}

#' Interval vector
#'
#' Computes a chord's interval vector, after
#' \insertCite{Parncutt2018;textual}{hrep}.
#'
#' @param x Object to analyse.
#' @return An integer vector of length 6,
#' where the ith element indicates the number of times that
#' the pitch-class interval i is found in the chord.
#' @details
#' An interval vector of class "int_vec",
#' describing how often each pitch-class interval
#' appears in a chord.
#' There are six possible interval classes: 1, 2, 3, 4, 5, and 6 semitones.
#' \insertCite{Parncutt2018;textual}{hrep} cite
#' \insertCite{Forte1977;textual}{hrep} for this technique.
#' @references
#'   \insertAllCited{}
#' @export
#' @rdname int_vec
#' @examples
#' int_vec(c(60, 64, 67)) # major triad
#' int_vec(c(60, 63, 66)) # diminished triad
int_vec <- function(x) {
  UseMethod("int_vec")
}

is.int_vec <- function(x) {
  is(x, "int_vec")
}

#' @rdname int_vec
#' @export
int_vec.pc_set <- function(x) {
  x <- as.numeric(x)
  res <- numeric(6L)
  if (length(x) != 0L) {
    for (i in seq(from = 1L, to = length(x) - 1L)) {
      for (j in seq(from = i, to = length(x))) {
        dist <- pc_dist(x[i], x[j])
        res[dist] <- res[dist] + 1L
      }
    }
  }
  .int_vec(res)
}

#' @rdname int_vec
#' @export
int_vec.default <- function(x) {
  int_vec(pc_set(x))
}

#' @export
print.int_vec <- function(x, ...) {
  cat("Interval vector: ", as.character(x), "\n", sep = "")
}

#' @rdname int_vec
#' @export
as.numeric.int_vec <- function(x, ...) {
  unclass(x)
}

#' @rdname int_vec
#' @export
as.character.int_vec <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

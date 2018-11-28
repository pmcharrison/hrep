#' @export
.int_vec <- function(x) {
  x <- as.integer(x)
  checkmate::qassert(x, "I6[0,)")
  class(x) <- c("int_vec", "integer")
  x
}

#' @export
int_vec <- function(x) {
  UseMethod("int_vec")
}

#' @export
is.int_vec <- function(x) {
  is(x, "int_vec")
}

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

#' @export
int_vec.default <- function(x) {
  int_vec(pc_set(x))
}

#' @export
print.int_vec <- function(x, ...) {
  cat("Interval vector: ", as.character(x), "\n", sep = "")
}

#' @export
as.numeric.int_vec <- function(x, ...) {
  unclass(x)
}

#' @export
as.character.int_vec <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

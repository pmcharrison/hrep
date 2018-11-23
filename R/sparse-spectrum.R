#' @export
sparse_spectrum <- function(x, y,
                            x_unit, y_unit,
                            label = "sparse spectrum",
                            x_lab = x_unit, y_lab = y_unit) {
  checkmate::qassert(x, "N")
  checkmate::qassert(y, "N")
  checkmate::qassert(x_unit, "S1")
  checkmate::qassert(y_unit, "S1")
  checkmate::qassert(label, "S1")
  checkmate::qassert(x_lab, "S1")
  checkmate::qassert(y_lab, "S1")
  stopifnot(length(x) == length(y))
  df <- data.frame(x = x, y = y)
  attr(df, "x_unit") <- x_unit
  attr(df, "y_unit") <- y_unit
  attr(df, "label") <- label
  attr(df, "x_lab") <- x_lab
  attr(df, "y_lab") <- y_lab
  class(df) <- c("sparse_spectrum", "data.frame")
  df
}

#' @export
is.sparse_spectrum <- function(x, ...) is(x, "sparse_spectrum")

#' @export
print.sparse_spectrum <- function(x, ...) {
  range <-
    cat(
      label(x), "\n",
      "  size = ", nrow(x), "\n",
      "  x = ", x_unit(x), "\n",
      "  y = ", y_unit(x), "\n\n",
      sep = ""
    )
}

#' @export
as.data.frame.sparse_spectrum <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
plot.sparse_spectrum <- function(x, ...) {
  df <- as.data.frame(x)
  n <- nrow(df)
  df2 <- data.frame(x = numeric(n * 3), y = numeric(n * 3))
  for (i in seq_len(n)) {
    I <- (i - 1L) * 3L
    df2$x[I + 1:3] <- df$x[i]
    df2$y[I + 2L] <- df$y[i]
  }
  plot(df2$x, df2$y, xlab = x_lab(x), ylab = y_lab(x), type = "l", ...)
}

#' @export
view.sparse_spectrum <- function(x, ...) plot(x, ...)

#' @export
x_unit.sparse_spectrum <- function(x) attr(x, "x_unit")

#' @export
y_unit.sparse_spectrum <- function(x) attr(x, "y_unit")

#' @export
`y_unit<-.sparse_spectrum` <- function(x, value) {
  checkmate::qassert(value, "S1")
  attr(x, "y_unit") <- value
  x
}

#' @export
label.sparse_spectrum <- function(x) attr(x, "label")

#' @export
x_lab.sparse_spectrum <- function(x) attr(x, "x_lab")

#' @export
y_lab.sparse_spectrum <- function(x) attr(x, "y_lab")

#' @export
`y_lab<-.sparse_spectrum` <- function(x, value) {
  checkmate::qassert(value, "S1")
  attr(x, "y_lab") <- value
  x
}

#' @export
transform_y.sparse_spectrum <- function(x, f, y_unit, y_lab) {
  stopifnot(is.function(f))
  checkmate::qassert(y_unit, "S1")
  checkmate::qassert(y_lab, "S1")
  x$y <- f(x$y)
  y_unit(x) <- y_unit
  y_lab(x) <- y_lab
  x
}

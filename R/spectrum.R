#' @export
spectrum <- function(x, x_unit, y_unit, lower, upper, low_eq, high_eq,
                     x_lab = x_unit, y_lab = y_unit) {
  checkmate::qassert(x, "N")
  checkmate::qassert(x_unit, "S1")
  checkmate::qassert(y_unit, "S1")
  checkmate::qassert(lower, "N1")
  checkmate::qassert(upper, "N1")
  checkmate::qassert(low_eq, "B1")
  checkmate::qassert(high_eq, "B1")
  checkmate::qassert(x_lab, "S1")
  checkmate::qassert(y_lab, "S1")
  attr(x, "x_unit") <- x_unit
  attr(x, "y_unit") <- y_unit
  attr(x, "lower") <- lower
  attr(x, "upper") <- upper
  attr(x, "low_eq") <- low_eq
  attr(x, "high_eq") <- high_eq
  attr(x, "x_lab") <- x_lab
  attr(x, "y_lab") <- y_lab
  class(x) <- "spectrum"
  x
}

#' @export
print.spectrum <- function(x, ...) {
  range <-
  cat(
    "spectrum\n",
    "  size = ", length(x), "\n",
    "  x = ", x_unit(x), "\n",
    "  range(x) = ",
    if (low_eq(x)) "[" else "(",
    lower(x), ", ", upper(x),
    if (high_eq(x)) "]" else ")",
    "\n",
    "  y = ", y_unit(x), "\n\n",
    sep = ""
  )
}

#' @export
as.data.frame.spectrum <- function(x, ...) {
  n <- length(x)
  n_seq <- n + (!low_eq(x)) + (!high_eq(x))
  x_seq <- seq(from = lower(x), to = upper(x), length.out = n_seq)
  if (!high_eq(x)) x_seq <- x_seq[- n_seq]
  if (!low_eq(x)) x_seq <- x_seq[- 1L]
  y_seq <- as.numeric(x)
  df <- data.frame(x = x_seq, y = y_seq)
  df
}

#' @export
plot.spectrum <- function(x, ...) {
  df <- as.data.frame(x)
  plot(df$x, df$y, xlab = x_lab(x), ylab = y_lab(x), type = "l", ...)
}

#' @export
x_unit.spectrum <- function(x) attr(x, "x_unit")

#' @export
y_unit.spectrum <- function(x) attr(x, "y_unit")

#' @export
lower.spectrum <- function(x) attr(x, "lower")

#' @export
upper.spectrum <- function(x) attr(x, "upper")

#' @export
low_eq.spectrum <- function(x) attr(x, "low_eq")

#' @export
high_eq.spectrum <- function(x) attr(x, "high_eq")

#' @export
x_lab.spectrum <- function(x) attr(x, "x_lab")

#' @export
y_lab.spectrum <- function(x) attr(x, "y_lab")



# sparse_spectrum

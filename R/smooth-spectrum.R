smooth_spectrum <- function(x, x_unit, y_unit, lower, upper, low_eq, high_eq,
                            label = "smooth spectrum",
                            x_lab = x_unit, y_lab = y_unit) {
  checkmate::qassert(x, "N")
  checkmate::qassert(x_unit, "S1")
  checkmate::qassert(y_unit, "S1")
  checkmate::qassert(lower, "N1")
  checkmate::qassert(upper, "N1")
  checkmate::qassert(low_eq, "B1")
  checkmate::qassert(high_eq, "B1")
  checkmate::qassert(label, "S1")
  checkmate::qassert(x_lab, "S1")
  checkmate::qassert(y_lab, "S1")
  attr(x, "x_unit") <- x_unit
  attr(x, "y_unit") <- y_unit
  attr(x, "lower") <- lower
  attr(x, "upper") <- upper
  attr(x, "low_eq") <- low_eq
  attr(x, "high_eq") <- high_eq
  attr(x, "label") <- label
  attr(x, "x_lab") <- x_lab
  attr(x, "y_lab") <- y_lab
  class(x) <- c("smooth_spectrum", "numeric")
  x
}

is.smooth_spectrum <- function(x, ...) is(x, "smooth_spectrum")

#' @export
c.smooth_spectrum <- function(...) {
  stop("no method exists for combining smooth spectra of this type")
}

#' @export
print.smooth_spectrum <- function(x, ...) {
  cat(
    label(x), "\n",
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
as.data.frame.smooth_spectrum <- function(x, ...) {
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
plot.smooth_spectrum <- function(x,
                                 ggplot = FALSE,
                                 xlim = NULL,
                                 ...) {
  df <- as.data.frame(x)
  if (ggplot) {
    assert_installed("ggplot2")
    if (!is.null(xlim)) df <- df[df$x >= xlim[1] & df$x <= xlim[2], ]
    ggplot2::ggplot(df, ggplot2::aes_string("x", "y")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(x_lab(x), limits = xlim) +
      ggplot2::scale_y_continuous(y_lab(x))
  } else {
    plot(df$x, df$y, xlab = x_lab(x), ylab = y_lab(x), type = "l", ...)
  }
}

#' @export
view.smooth_spectrum <- function(x, ...) {
  utils::View(as.data.frame(x, ...))
}

#' @export
x_unit.smooth_spectrum <- function(x) attr(x, "x_unit")

#' @export
y_unit.smooth_spectrum <- function(x) attr(x, "y_unit")

#' @export
lower.smooth_spectrum <- function(x) attr(x, "lower")

#' @export
upper.smooth_spectrum <- function(x) attr(x, "upper")

#' @export
low_eq.smooth_spectrum <- function(x) attr(x, "low_eq")

#' @export
high_eq.smooth_spectrum <- function(x) attr(x, "high_eq")

#' @export
label.smooth_spectrum <- function(x) attr(x, "label")

#' @export
x_lab.smooth_spectrum <- function(x) attr(x, "x_lab")

#' @export
y_lab.smooth_spectrum <- function(x) attr(x, "y_lab")

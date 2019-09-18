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
  data.frame(x = x$x,
             y = x$y)
}

#' @export
plot.sparse_spectrum <- function(x, ggplot = FALSE, xlim = NULL, ...) {
  df <- as.data.frame(x)
  n <- nrow(df)
  df2 <- data.frame(x = numeric(n * 3), y = numeric(n * 3))
  for (i in seq_len(n)) {
    I <- (i - 1L) * 3L
    df2$x[I + 1:3] <- df$x[i]
    df2$y[I + 2L] <- df$y[i]
  }
  if (ggplot) {
    tibble::tibble(x = df2$x, y = df2$y) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(x_lab(x), limits = xlim) +
      ggplot2::scale_y_continuous(y_lab(x))
  } else {
    plot(df2$x, df2$y, xlab = x_lab(x), ylab = y_lab(x),
         type = "l", xlim = xlim, ...)
  }
}

#' @export
view.sparse_spectrum <- function(x, ...) {
  utils::View(as.data.frame(x, ...))
}

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

transform_y.sparse_spectrum <- function(x, f, y_unit, y_lab) {
  stopifnot(is.function(f))
  checkmate::qassert(y_unit, "S1")
  checkmate::qassert(y_lab, "S1")
  x$y <- f(x$y)
  y_unit(x) <- y_unit
  y_lab(x) <- y_lab
  x
}

#' Combine sparse spectra
#'
#' This function combines a series of sparse spectra into one spectrum
#' assuming incoherent amplitude summation.
#' This involves a rounding process,
#' by which the MIDI pitch(-class) of each partial
#' is rounded to a specified number of digits.
#'
#' @param ... Sparse spectra to combine
#' (see \code{\link{sparse_pi_spectrum}} and \code{\link{sparse_pc_spectrum}}).
#'
#' @param digits
#' (Integerish scalar)
#' The MIDI pitch(-class) of each partial will be rounded to this number
#' of digits.
#'
#' @return A sparse spectrum object.
#'
#' @export
combine_sparse_spectra <- function(..., digits = 6) {
  checkmate::qassert(digits, "X1[0,)")
  input <- list(...)
  if (length(input) == 0) stop("combine_sparse_spectra needs at least 1 input")
  if (length(input) == 1) return(input[[1]])
  if (!all(purrr::map_lgl(input,
                          ~ is.sparse_pi_spectrum(.) |
                          is.sparse_fr_spectrum(.) |
                          is.sparse_pc_spectrum(.))))
    stop("all inputs must be one of ",
         "sparse_pi_spectrum, ",
         "sparse_fr_spectrum, or ",
         "sparse_pc_spectrum")

  output_class <- intersect(class(input[[1]]),
                            c("sparse_pi_spectrum",
                              "sparse_fr_spectrum",
                              "sparse_pc_spectrum"))

  octave_invariant <- is.sparse_pc_spectrum(input[[1]])
  if (octave_invariant &&
      !all(purrr::map_lgl(input, is.sparse_pc_spectrum)))
    stop("cannot mix sparse_pc_spectrum inputs with",
         "sparse_fr_spectrum and sparse_pi_spectrum inputs")
  input <- if (octave_invariant)
    purrr::map(input, sparse_pc_spectrum) else
      purrr::map(input, sparse_pi_spectrum)

  res <-
    lapply(input, as.data.frame) %>%
    collapse_summing_amplitudes(digits = digits) %>%
    {
      f <- if (octave_invariant) .sparse_pc_spectrum else .sparse_pi_spectrum
      f(.$x, .$y)
    }

  if (output_class == "sparse_fr_spectrum") sparse_fr_spectrum(res) else res
}

collapse_summing_amplitudes <- function(x, digits, modulo = NA_real_) {
  checkmate::qassert(modulo, "n1(0,)")
  if (!is.list(x) ||
      !all(purrr::map_lgl(x, ~ is.data.frame(.) &&
                          identical(names(.), c("x", "y")))))
    stop("x must be a list of data frames with columns 'x' and 'y'")
  x %>%
    data.table::rbindlist() %>%
    {
      .$x <- round(.$x, digits = digits)
      if (!is.na(modulo)) .$x <- .$x %% modulo
      .
    } %>%
    {reduce_by_key(
      keys = .$x,
      values = .$y,
      function(x, y) sum_amplitudes(x, y, coherent = FALSE),
      key_type = "numeric"
    )} %>%
    magrittr::set_names(c("x", "y"))
}

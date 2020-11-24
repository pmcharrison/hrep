sparse_spectrum <- function(x, y,
                            x_unit, y_unit,
                            label = "sparse spectrum",
                            x_lab = x_unit, y_lab = y_unit,
                            labels = NULL) {
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
  if (!is.null(labels)) df <- set_labels(df, labels)
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
      "  y = ", y_unit(x), "\n",
      if (!is.null(x$labels)) "  (has labels)\n",
      "\n",
      sep = ""
    )
}

#' @export
as.data.frame.sparse_spectrum <- function(x, ...) {
  df <- data.frame(x = x$x, y = x$y)
  if (!is.null(x$labels)) df$labels <- x$labels
  df
}

#' Add labels
#'
#' Adds labels to an object, typically for plotting purposes.
#'
#' @param x Object.
#'
#' @param labels Character vector of labels to add to the object.
#' In the case of sparse spectra, there should be one label for each partial.
#'
#' @return The original object, with labels added.
#'
#' @rdname set_labels
#'
#' @examples
#' spectrum <- sparse_pi_spectrum("60 64 67", num_harmonics = 1)
#' labels <- as.character(c(1, 2, 3))
#' spectrum_with_labels <- set_labels(spectrum, labels)
#' plot(spectrum_with_labels)
#'
#' @export
set_labels <- function(x, labels) {
  UseMethod("set_labels")
}

#' @rdname set_labels
#' @export
set_labels.sparse_spectrum <- function(x, labels) {
  checkmate::qassert(labels, sprintf("S%i", nrow(x)))
  x$labels <- labels
  x
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
    assert_installed("ggplot2")
    tibble::tibble(x = df2$x, y = df2$y) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(x_lab(x), limits = xlim) +
      ggplot2::scale_y_continuous(y_lab(x))
  } else {
    plot(df2$x, df2$y, xlab = x_lab(x), ylab = y_lab(x),
         type = "l", xlim = xlim, ...)
    if (!is.null(df$labels)) {
      for (i in seq_len(nrow(df))) {
        graphics::text(df$x[i], 0, rep("\u2588", times = nchar(df$label[i])) %>% paste(collapse = ""), col = "white")
        graphics::text(df$x[i], 0, df$label[i])
      }
    }
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
      f(.$x, .$y, labels = .$labels)
    }

  if (output_class == "sparse_fr_spectrum") sparse_fr_spectrum(res) else res
}

collapse_summing_amplitudes <- function(x, digits, modulo = NA_real_) {
  checkmate::qassert(modulo, "n1(0,)")
  if (!is.list(x) ||
      !all(purrr::map_lgl(x, ~ is.data.frame(.) &&
                          all(c("x", "y") %in% names(.)))))
    stop("x must be a list of data frames with columns 'x' and 'y'")
  has_labels <- !is.null(x[[1]]$labels)

  x %>%
    data.table::rbindlist() %>%
    {
      .$x <- round(.$x, digits = digits)
      if (!is.na(modulo)) .$x <- .$x %% modulo
      .
    } %>%
    {reduce_by_key(
      keys = .$x,
      values = if (has_labels) purrr::map2(.$y, .$labels, ~ list(amplitude = .x, label = .y)) else .$y,
      function(x, y) {
        if (has_labels) {
          list(amplitude = sum_amplitudes(x, y$amplitude, coherent = FALSE),
               label = y$label)
        } else {
          sum_amplitudes(x, y, coherent = FALSE)
        }
      },
      key_type = "numeric"
    )} %>% {
      if (has_labels) {
        list(x = .[[1]],
             y = purrr::map_dbl(.[[2]], "amplitude"),
             labels = purrr::map_chr(.[[2]], "label"))
      } else {
        magrittr::set_names(., c("x", "y"))
      }
    }
}

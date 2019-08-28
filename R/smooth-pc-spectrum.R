#' @export
.smooth_pc_spectrum <- function(x) {
  checkmate::qassert(x, "N1200")
  x <- unclass(x)
  y <- smooth_spectrum(x = x,
                       x_unit = "pc",
                       y_unit = "weight",
                       lower = 0,
                       upper = 12,
                       low_eq = TRUE,
                       high_eq = FALSE,
                       label = "smooth pitch-class spectrum",
                       x_lab = "Pitch class",
                       y_lab = "Weight")
  class(y) <- c("smooth_pc_spectrum", "chord", class(y))
  y
}

#' @export
is.smooth_pc_spectrum <- function(x) {
  is(x, "smooth_pc_spectrum")
}

#' @export
smooth_pc_spectrum <- function(x, sigma = 6.83, ...) {
  UseMethod("smooth_pc_spectrum")
}

#' @export
smooth_pc_spectrum.default <- function(x, sigma = 6.83, ...) {
  smooth_pc_spectrum(sparse_pc_spectrum(x, ...), sigma = sigma)
}

#' @export
smooth_pc_spectrum.sparse_pc_spectrum <- function(x, sigma = 6.83) {
  df <- collapse_summing_amplitudes(list(x), digits = 2, modulo = 12)
  df$ind <- 1 + df$x * 100

  checkmate::qassert(df$ind, "X[1,12000]")
  stopifnot(!anyDuplicated(df$ind))

  vec <- numeric(1200)
  vec[df$ind] <- df$y

  template <- smooth_gaussian_template(sigma, dim = 1200)

  .smooth_pc_spectrum(
    stats::convolve(vec,
                    template,
                    type = "circular")[c(601:1200, 1:600)]
  )
}

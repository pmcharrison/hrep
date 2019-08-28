#' @export
.smooth_pi_spectrum <- function(x) {
  checkmate::qassert(x, "N12000")
  x <- unclass(x)
  y <- smooth_spectrum(x = x,
                       x_unit = "pi",
                       y_unit = "weight",
                       lower = 0,
                       upper = 120,
                       low_eq = TRUE,
                       high_eq = FALSE,
                       label = "smooth pitch spectrum",
                       x_lab = "Pitch (MIDI)",
                       y_lab = "Weight")
  class(y) <- c("smooth_pi_spectrum", "chord", class(y))
  y
}

#' @export
is.smooth_pi_spectrum <- function(x) {
  is(x, "smooth_pi_spectrum")
}

#' @export
smooth_pi_spectrum <- function(x, sigma = 6.83, ...) {
  UseMethod("smooth_pi_spectrum")
}

#' @export
smooth_pi_spectrum.default <- function(x, sigma = 6.83, ...) {
  smooth_pi_spectrum(sparse_pi_spectrum(x, ...), sigma = sigma)
}

#' @export
smooth_pi_spectrum.sparse_pi_spectrum <- function(x, sigma = 6.83) {
  df <- collapse_summing_amplitudes(list(x), digits = 2)
  df$ind <- 1 + df$x * 100

  checkmate::qassert(df$ind, "X[1,12000]")
  stopifnot(!anyDuplicated(df$ind))

  vec <- numeric(12000)
  vec[df$ind] <- df$y

  template <- smooth_gaussian_template(sigma, dim = 12000)

  .smooth_pi_spectrum(
    stats::convolve(vec,
                    template,
                    type = "open")[seq(from = 6000L, length = 12000L)]
  )
}

smooth_gaussian_template <- function(sigma, dim) {
  stopifnot(dim %% 2 == 0)
  ind <- seq_len(dim)
  centre_ind <- 1L + dim / 2L
  dnorm(ind, mean = centre_ind, sd = sigma)
}

smooth_gaussian_template <- memoise::memoise(smooth_gaussian_template)

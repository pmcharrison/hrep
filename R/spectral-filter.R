#' Spectral filter
#'
#' Filters a sparse spectrum using a user-provided function.
#'
#' @param x Object to filter, passed to \code{\link{sparse_fr_spectrum}}.
#'
#' @param fun Function to apply. The input should be a vector of frequencies,
#' the output should be a vector of envelope amplitudes for those frequencies.
#' An amplitude of 1 means that the spectrum is left unchanged at that location.
#'
#' @param ... Optional parameters passed to \code{\link{sparse_fr_spectrum}}.
#'
#' @export
filter_spectrum <- function(x, fun, ...) {
  UseMethod("filter_spectrum")
}

#' @export
filter_spectrum.default <- function(x, fun, ...) {
  filter_spectrum.sparse_fr_spectrum(sparse_fr_spectrum(x, ...), fun)
}

#' @export
filter_spectrum.sparse_fr_spectrum <- function(x, fun, ...) {
  x$y <- x$y * fun(x$x)
  x
}

#' Gaussian filter
#'
#' Applies a Gaussian filter to a sparse spectrum.
#'
#' @param location
#' Vector of locations for the Gaussians.
#'
#' @param width
#' Vector of widths (i.e. SDs) for the Gaussians.
#' If only one number is provided, this same number is used for all Gaussians.
#'
#' @inheritParams filter_spectrum
#'
#' @export
gaussian_filter <- function(x, location, width = 50, ...) {
  stopifnot(length(location) > 0,
            length(width) == 1 || length(width) == length(location))
  if (length(width) == 1) {
    width <- rep(width, times = length(location))
  }
  f <- function(x) {
    purrr::map2(location, width, function(.location, .width) {
      dens <- stats::dnorm(x, mean = .location, sd = .width)
      dens / max(dens)
    }) %>%
      do.call(rbind, .) %>%
      colSums()
  }
  filter_spectrum(x, f, ...)
}

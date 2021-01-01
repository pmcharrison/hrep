#' Filter spectrum
#'
#' Filters a sparse spectrum using a user-provided function.
#'
#' @param x Object to filter, passed to \code{\link{sparse_fr_spectrum}}.
#'
#' @param fun Function to apply. The input should be a vector of frequencies,
#' the output should be a vector of envelope amplitudes for those frequencies.
#' The spectral amplitudes will be multiplied by the outputs of this function
#' for each frequency.
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
#' @param base
#' (Numeric scalar, default = 0)
#' Baseline upon which the Gaussian sits.
#' If this is set to 1, then partials outside the range of the Gaussian
#' will be unaffected by the filter; if this is left at 0,
#' then these partials will be removed.
#'
#' @param peak
#' (Numeric scalar, default = 0)
#' Notional peak of the Gaussian.
#'
#' @inheritParams filter_spectrum
#'
#' @export
filter_spectrum_gaussian <- function(x,
                                     location,
                                     width = 50,
                                     base = 0,
                                     peak = 1,
                                     ...) {
  stopifnot(length(location) > 0,
            length(width) == 1 || length(width) == length(location),
            length(base) == 1,
            length(peak) == 1,
            base <= peak)
  if (length(width) == 1) {
    width <- rep(width, times = length(location))
  }
  f <- function(x) {
    purrr::map2(location, width, function(.location, .width) {
      base + (peak - base) * exp(- (x - .location) ^ 2 / (2 * .width ^ 2))
    }) %>%
      do.call(rbind, .) %>%
      colSums()
  }
  filter_spectrum(x, f, ...)
}

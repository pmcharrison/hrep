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

#' Is smooth pitch-class spectrum
#'
#' Checks whether an object belongs to the class \code{smooth_pc_spectrum}.
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is.smooth_pc_spectrum <- function(x) {
  is(x, "smooth_pc_spectrum")
}

#' Smooth pitch-class spectrum
#'
#' Creates an object of class \code{smooth_pc_spectrum},
#' describing a smooth pitch-class spectrum.
#' A smooth pitch-class spectrum describes perceptual weight
#' along a continuous pitch class scale.
#' The representation incorporates smoothing to account for
#' imprecisions in pitch perception.
#'
#' This representation is created by expressing the sonority
#' as a sparse pitch-class spectrum (see \code{\link{sparse_pc_spectrum}})
#' and convolving the result with a Gaussian distribution with standard deviation
#' \code{sigma}.
#'
#' @param x Object to convert. By default \code{\link{sparse_pc_spectrum}}
#' is called first to convert the object to a sparse pitch-class spectrum.
#'
#' @param sigma
#' (Numeric scalar)
#' Standard deviation of the Gaussian distribution used to simulate
#' perceptual blurring. Defaults to 6.83 cents, after
#' \insertCite{Milne2016;textual}{hrep}.
#'
#' @param ...
#' Provided for S3 method consistency.
#'
#' @inheritParams expand_harmonics
#'
#' @seealso
#' This representation was inspired by \code{\link{milne_pc_spectrum}},
#' which embodies similar ideas with a slightly different implementation.
#' See \code{\link{smooth_pi_spectrum}} for an equivalent representation
#' in the pitch domain.
#'
#' @rdname smooth_pc_spectrum
#'
#' @export
smooth_pc_spectrum <- function(x, sigma = 6.83, ...) {
  ellipsis::check_dots_used()
  UseMethod("smooth_pc_spectrum")
}

#' @rdname smooth_pc_spectrum
#' @export
smooth_pc_spectrum.default <- function(x,
                                       sigma = 6.83,
                                       num_harmonics = 11L,
                                       roll_off = 1,
                                       ...) {
  smooth_pc_spectrum(sparse_pc_spectrum(x,
                                        num_harmonics = num_harmonics,
                                        roll_off = roll_off),
                     sigma = sigma, ...)
}

#' @rdname smooth_pc_spectrum
#' @export
smooth_pc_spectrum.sparse_pc_spectrum <- function(x, sigma = 6.83, ...) {
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

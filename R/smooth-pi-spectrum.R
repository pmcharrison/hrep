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

#' Is smooth pitch spectrum
#'
#' Checks whether an object belongs to the class \code{smooth_pi_spectrum}.
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is.smooth_pi_spectrum <- function(x) {
  is(x, "smooth_pi_spectrum")
}

#' Smooth pitch spectrum
#'
#' Creates an object of class \code{smooth_pi_spectrum},
#' describing a smooth pitch spectrum.
#' A smooth pitch spectrum describes perceptual weight
#' along a continuous MIDI pitch scale.
#' The representation incorporates smoothing to account for
#' imprecisions in pitch perception.
#'
#' This representation is created by expressing the sonority
#' as a sparse pitch spectrum (see \code{\link{sparse_pi_spectrum}})
#' and convolving the result with a Gaussian distribution with standard deviation
#' \code{sigma}.
#'
#' @param x Object to convert. By default \code{\link{sparse_pi_spectrum}}
#' is called first to convert the object to a sparse pitch spectrum.
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
#' See \code{\link{smooth_pc_spectrum}} for an equivalent representation
#' in the pitch-class domain.
#'
#' @rdname smooth_pi_spectrum
#'
#' @export
smooth_pi_spectrum <- function(x, sigma = 6.83, ...) {
  ellipsis::check_dots_used()
  UseMethod("smooth_pi_spectrum")
}

#' @rdname smooth_pi_spectrum
#' @export
smooth_pi_spectrum.default <- function(x,
                                       sigma = 6.83,
                                       num_harmonics = 11L,
                                       roll_off = 1,
                                       ...) {
  smooth_pi_spectrum(sparse_pi_spectrum(x,
                                        num_harmonics = num_harmonics,
                                        roll_off = roll_off,
                                        ...),
                     sigma = sigma)
}

#' @rdname smooth_pi_spectrum
#' @export
smooth_pi_spectrum.sparse_pi_spectrum <- function(x, sigma = 6.83, ...) {
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

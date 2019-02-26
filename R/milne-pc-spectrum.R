#' Constructor function for Milne pitch-class spectrum
#'
#' This function constructs a "milne_pc_spectrum" object.
#' @param x A numeric vector of pitch-class weights,
#' typically (but not necessarily) of length 1200.
#' @return An object of class "milne_pc_spectrum".
#' @seealso \code{\link{milne_pc_spectrum}}.
#' @export
.milne_pc_spectrum <- function(x) {
  checkmate::qassert(x, "N")
  x <- unclass(x)
  y <- smooth_spectrum(x = x,
                       x_unit = "pc",
                       y_unit = "weight",
                       lower = 0,
                       upper = 12,
                       low_eq = TRUE,
                       high_eq = FALSE,
                       label = "pitch-class spectrum",
                       x_lab = "Pitch class",
                       y_lab = "Weight")
  class(y) <- c("milne_pc_spectrum", class(y))
  y
}

#' Milne pitch-class spectrum
#'
#' This function represents an input object as a
#' 'Milne pitch-class spectrum'.
#' A Milne pitch-class spectrum defines 'perceptual weight'
#' as a continuous function of 'pitch class'.
#' @details
#' This spectrum is typically constructed from musical chords
#' by expanding each note into its implied harmonics
#' and applying a Gaussian smoothing to account for perceptual uncertainties.
#' See \insertCite{Milne2016;textual}{hrep} for details.
#' @param x Input sonority.
#' @param ... Further arguments passed to specific methods.
#' @return An object of class \code{milne_pc_spectrum}.
#' @rdname milne_pc_spectrum
#' @seealso \code{\link{.milne_pc_spectrum}}.
#' @export
milne_pc_spectrum <- function(x, ...) {
  UseMethod("milne_pc_spectrum")
}

#' @param weights (Numeric vector)
#' Vector of weights to assign to each pitch class.
#' If a scalar value is provided, this value is assigned to all pitch classes.
#' @param num_harmonics (Integerish scalar)
#' Number of harmonics to use when expanding tones into their implied harmonics,
#' and when defining the harmonic template
#' (including the fundamental frequency).
#' Defaults to 12, after
#' \insertCite{Milne2016;textual}{hrep}.
#' @param rho (Numeric scalar)
#' Roll-off parameter for harmonic expansion.
#' Defaults to 0.75, after
#' \insertCite{Milne2016;textual}{hrep}.
#' @param sigma (Numeric scalar)
#' Standard deviation of the Gaussian smoothing distribution (cents).
#' Defaults to 6.83, after
#' \insertCite{Milne2016;textual}{hrep}.
#' @param array_dim (Integerish scalar)
#' Dimensionality of the pitch-class spectrum array.
#' Defaults to 1200, after
#' \insertCite{Milne2016;textual}{hrep}.
#' @rdname milne_pc_spectrum
#' @references
#' \insertAllCited{}
#' @export
milne_pc_spectrum.pc_set <- function(x,
                                     weights = 1,
                                     num_harmonics = 12,
                                     rho = 0.75,
                                     sigma = 6.83,
                                     array_dim = 1200,
                                     ...) {
  if (length(weights) == 1L) weights <- rep(weights, times = length(x))
  pc_spectra <- mapply(
    function(pc, weight) {
      get_complex_tone(fundamental_pc = pc,
                       weight = weight,
                       array_dim = array_dim,
                       num_harmonics = num_harmonics,
                       rho = rho,
                       sigma = sigma)
    }, x, weights)
  .milne_pc_spectrum(rowSums(pc_spectra))
}

#' @rdname milne_pc_spectrum
#' @export
milne_pc_spectrum.default <- function(x, ...) {
  milne_pc_spectrum(pc_set(x), ...)
}

#' Check for class "milne_pc_spectrum"
#'
#' Checks whether an object is of class "milne_pc_spectrum".
#' @param x Object to check.
#' @return Logical scalar.
#' @export
is.milne_pc_spectrum <- function(x) is(x, "milne_pc_spectrum")


# Pitch-class spectrum, template 1
# Makes a Gaussian pitch-class  spectral template with unit mass, centred on 0,
# with standard deviation <sigma>.
# The template will be truncated to zero for points <truncation-point>
# standard deviations or further away from the mean,
# after \insertCite{Milne2016;textual}{hrep}.
pc_spectrum_template_1 <- function(array_dim, sigma, truncation_point) {
  checkmate::qassert(array_dim, "X1[3,)")
  checkmate::qassert(sigma, "N1")
  checkmate::qassert(truncation_point, "N1(0,]")

  limit <- floor(sigma * 12)
  template <- numeric(array_dim)
  template[1] <- dnorm(0, mean = 0, sd = sigma)
  seq <- seq(from = 1, to = limit)
  weight <- dnorm(seq, mean = 0, sd = sigma)
  template[2:(limit + 1)] <- weight
  template[array_dim:(array_dim - limit + 1)] <- weight
  template
}

pc_spectrum_template_2 <- function(array_dim, mean, mass, sigma, truncation_point = 12) {
  stopifnot(mean >= 0, mean <= array_dim)
  origin <- round(mean)
  template <- pc_spectrum_template_1(
    array_dim, sigma, truncation_point = truncation_point
  )
  scaled <- template * mass
  output <- numeric(array_dim)
  seq <- seq(from = 0, to = array_dim - 1)
  output <- scaled[((seq - origin) %% array_dim) + 1]
  output
}

#' Make complex tone
#'
#' Returns an array describing the pitch-class spectrum for a given complex tone.
#' @param num_harmonics Number of harmonics, including the fundamental
#' @keywords internal
new_complex_tone <- function(
  fundamental_pc,
  weight,
  array_dim,
  num_harmonics,
  rho,
  sigma
) {
  checkmate::qassert(fundamental_pc, "N1[0,12)")
  checkmate::qassert(weight, "N1[0,)")
  checkmate::qassert(num_harmonics, "X1[0,)")
  checkmate::qassert(rho, "N1")
  checkmate::qassert(sigma, "N1[0,)")
  pcs <- vapply(seq_len(num_harmonics),
                function(i) {
                  (
                    (fundamental_pc * array_dim / 12) +
                      (array_dim * log(i, 2))
                  ) %% array_dim
                }, numeric(1))
  weights <- vapply(seq_len(num_harmonics),
                    function(i) {
                      weight / (i ^ rho)
                    }, numeric(1))
  spectra <- mapply(
    function(pc, weight) {
      pc_spectrum_template_2(array_dim, pc, weight, sigma)
    }, pcs, weights, SIMPLIFY = TRUE
  )
  spectrum <- rowSums(spectra)
  spectrum
}

# Get complex tone
#
# Wrapper for \code{new_complex_tone} that implements caching.
get_complex_tone <- memoise::memoise(new_complex_tone)

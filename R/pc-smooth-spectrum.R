#' Convert to pitch-class spectrum
#' Converts the input to a pitch-class spectrum.
#' Musical notes are converted to complex tones, after Milne & Holland (2016).
#' @param num_harmonics The number of harmonics in each modelled complex tone
#' (including the fundamental) (numeric scalar)
#' @param rho The roll-off parameter determining the weight of
#'  successive harmonics; default is 0.75, after Milne & Holland (2016)
#' @param sigma The standard deviation of the smoothing Gaussian;
#' default is 6.83, after Milne & Holland (2016)
#' @export
#' @references
#' \insertRef{Milne2016a}{hrep}
pc_smooth_spectrum <- function(x,
                               weights = 1,
                               array_dim = 1200,
                               num_harmonics = 12,
                               rho = 0.75,
                               sigma = 6.83,
                               ...) {
  stopifnot(is.pc_set(x))
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
  as.pc_smooth_spectrum(rowSums(pc_spectra))
}

#' @export
is.pc_smooth_spectrum <- function(x) is(x, "pc_smooth_spectrum")

#' @export
as.pc_smooth_spectrum <- function(x, ...) UseMethod("as.pc_smooth_spectrum")

#' @export
as.pc_smooth_spectrum.pc_smooth_spectrum <- function(x, ...) x

#' @export
as.pc_smooth_spectrum.numeric <- function(x, ...) {
  checkmate::qassert(x, "N")
  if (is.spectrum(x)) stop("<x> is already a spectrum")
  y <- smooth_spectrum(x = x,
                       x_unit = "pc",
                       y_unit = "weight",
                       lower = 0,
                       upper = 12,
                       low_eq = TRUE,
                       high_eq = FALSE,
                       x_lab = "Pitch class",
                       y_lab = "Weight")
  class(y) <- c("pc_smooth_spectrum", class(y))
  y
}

#' @export
as.pc_smooth_spectrum.pc_set <- function(x, ...) {
  pc_smooth_spectrum(x, ...)
}

#' @export
as.pc_smooth_spectrum.pi_chord <- function(x, ...) {
  pc_smooth_spectrum(as.pc_set(x), ...)
}

#' @export
as.pc_smooth_spectrum.pc_chord <- function(x, ...) {
  pc_smooth_spectrum(as.pc_set(x), ...)
}


#' @export
print.pc_smooth_spectrum <- function(x, ...) {
  cat("Pitch-class spectrum ",
      "(N = ", length(x), ", ",
      "M = ",  mean(x, na.rm = FALSE) %>%
        round(digits = 3), ", ",
      "SD = ",  sd(x, na.rm = FALSE) %>%
        round(digits = 3), ")",
      sep = "")
}

#' Pitch-class spectrum, template 1
#' Makes a Gaussian pitch-class  spectral template with unit mass, centred on 0,
#' with standard deviation <sigma>.
#' The template will be truncated to zero for points <truncation-point>
#' standard deviations or further away from the mean,
#' after \insertCite{Milne2016a;textual}{hrep}.
#' @references
#'   \insertAllCited{}
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

#' Get complex tone
#'
#' Wrapper for \code{new_complex_tone} that implements caching.
get_complex_tone <- memoise::memoise(new_complex_tone)

#' @export
get_cosine_similarity <- function(x, y) {
  numerator <- sum(x * y)
  denominator <-
    sqrt(sum(x ^ 2)) *
    sqrt(sum(y ^ 2))
  numerator / denominator
}

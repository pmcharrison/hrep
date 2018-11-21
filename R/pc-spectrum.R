#' @export
pc_spectrum <- function(x) {
  checkmate::qassert(x, "R+")
  x <- as.numeric(x)
  class(x) <- c("pc_spectrum", "numeric")
  x
}

#' @export
print.pc_spectrum <- function(x, ...) {
  cat("Pitch-class spectrum ",
      "(N = ", length(x), ", ",
      "M = ",  mean(x, na.rm = FALSE) %>%
        round(digits = 3), ", ",
      "SD = ",  sd(x, na.rm = FALSE) %>%
        round(digits = 3), ")",
      sep = "")
}

#' @export
as.data.frame.pc_spectrum <- function(x, ...) {
  n <- length(x)
  pc <- seq(from = 0, to = 12, length.out = n + 1)[- n]
  data.frame(pitch_class = pc, weight = as.numeric(x))
}

#' @export
plot.pc_spectrum <- function(x, ...) {
  df <- as.data.frame(x)
  plot(df$pitch_class, df$weight,
       type = "l",
       xlab = "Pitch class",
       ylab = "Weight"
  )
}

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
#' \insertRef{Milne2016a}{hutil}
as.pc_spectrum <- function(x,
                           weights = 1,
                           array_dim = 1200,
                           num_harmonics = 12,
                           rho = 0.75,
                           sigma = 6.83,
                           ...) {
  UseMethod("as.pc_spectrum")
}

#' @export
as.pc_spectrum.pc_set <- function(x,
                                  weights = 1,
                                  array_dim = 1200,
                                  num_harmonics = 12,
                                  rho = 0.75,
                                  sigma = 6.83) {
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
  pc_spectrum(rowSums(pc_spectra))
}

#' Pitch-class spectrum, template 1
#' Makes a Gaussian pitch-class  spectral template with unit mass, centred on 0,
#' with standard deviation <sigma>.
#' The template will be truncated to zero for points <truncation-point>
#' standard deviations or further away from the mean,
#' after \insertCite{Milne2016a;textual}{hutil}.
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

#' #' This function builds a comprehensive memory-based cache for
#' #' convert_pc_set_to_pc_spectrum.
#' #' @export
#' cache_convert_pc_set_to_pc_spectrum <- function(
#'   array_dim = 1200,
#'   num_harmonics = 12,
#'   rho = 0.75,
#'   sigma = 6.83
#' ) {
#'   alphabet <- pc_set_alphabet$by_id
#'   cache_convert_pc_set_to_pc_spectrum <- new.env()
#'   pb <- txtProgressBar(max = length(alphabet), style = 3)
#'   for (i in seq_along(alphabet)) {
#'     pc_set <- alphabet[[i]]
#'     convert_pc_set_to_pc_spectrum(
#'       pc_set,
#'       array_dim = array_dim,
#'       num_harmonics = num_harmonics,
#'       rho = rho,
#'       sigma = sigma,
#'       cache = TRUE,
#'       cache_env = cache_convert_pc_set_to_pc_spectrum
#'     )
#'     setTxtProgressBar(pb, i)
#'   }
#'   cache_convert_pc_set_to_pc_spectrum
#' }

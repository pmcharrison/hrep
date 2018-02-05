#' @export
setClass("PCSpectrum",
         slots = list(
           values = "numeric"
         ),
         prototype = list(
           values = numeric()
         )
)

#' @export
make_pc_spectrum <- function(values) {
  new("PCSpectrum", values = values)
}

#' @export
setMethod(
  "show", signature(object = "PCSpectrum"),
  function(object) {
    cat("Pitch-class spectrum ",
        "(N = ", length(object@values), ", ",
        "M = ",  mean(object@values, na.rm = FALSE) %>%
          round(digits = 3), ", ",
        "SD = ",  sd(object@values, na.rm = FALSE) %>%
          round(digits = 3), ")",
        sep = "")
  }
)

#' @export
setMethod(
  "plot", signature(x = "PCSpectrum"),
  function(x, ...) {
    n <- length(x@values)
    pc <- seq(from = 0, to = 12, length.out = n + 1)[- n]
    plot(pc, x@values,
         type = "l",
         xlab = "Pitch class",
         ylab = "Salience"
    )
  }
)

#' Convert pitch-class set to pitch-class spectrum
#' Takes a list of pitch classes and outputs a pitch-class spectrum obtained by treating each of the pitch classes as complex tones, after Milne & Holland (2016).
#' @param num_harmonics The number of harmonics in each modelled complex tone (including the fundamental) (numeric scalar)
#' @param rho The roll-off parameter determining the salience of successive harmonics; default is 0.75, after Milne & Holland (2016)
#' @param sigma The standard deviation of the smoothing Gaussian; default is 6.83, after Milne & Holland (2016)
#' @export
#' @references
#' \insertRef{Milne2016a}{HarmonyDistance}
convert_pc_set_to_pc_spectrum <- function(
  pc_set,
  array_dim = 1200,
  num_harmonics = 12,
  rho = 0.75,
  sigma = 6.83,
  cache = TRUE,
  cache_env = NULL,
  cache_stop_on_missing = FALSE
) {
  pc_set <- as.numeric(sort(pc_set))
  cacheR::cache(
    fun_name = "convert_pc_set_to_pc_spectrum",
    cache = cache,
    cache_env = cache_env,
    cache_root = "cache",
    cache_dir = "HarmonyUtils/convert_pc_set_to_pc_spectrum",
    stop_on_missing = cache_stop_on_missing,
    ignore_args = c("cache", "cache_env", "cache_stop_on_missing"),
    expr = expression({
      assertthat::assert_that(
        all(pc_set == round(pc_set)),
        !anyDuplicated(pc_set)
      )
      pc_spectra <- sapply(pc_set, function(pc) {
        get_complex_tone(fundamental_pc = pc,
                         array_dim = array_dim,
                         num_harmonics = num_harmonics,
                         rho = rho,
                         sigma = sigma)
      })
      pc_spectrum <- rowSums(pc_spectra)
      new("PCSpectrum", values = pc_spectrum)
    }))
}

#' This function builds a comprehensive memory-based cache for
#' convert_pc_set_to_pc_spectrum.
#' @export
cache_convert_pc_set_to_pc_spectrum <- function() {
  alphabet <- HarmonyUtils::pc_set_alphabet$by_id
  cache_convert_pc_set_to_pc_spectrum <- new.env()
  pb <- txtProgressBar(max = length(alphabet), style = 3)
  for (i in seq_along(alphabet)) {
    pc_set <- alphabet[[i]]
    HarmonyUtils::convert_pc_set_to_pc_spectrum(
      pc_set, cache = TRUE,
      cache_env = cache_convert_pc_set_to_pc_spectrum
    )
    setTxtProgressBar(pb, i)
  }
  cache_convert_pc_set_to_pc_spectrum
}

#' Make Gaussian spectral template
#'
#' Makes a Gaussian spectral template with unit mass, centred on 0, with standard deviation <sigma>. The template will be truncated to zero for points <truncation-point> standard deviations or further away from the mean, after Milne's implementation.
make_gaussian_spectral_template <- function(array_dim, sigma, truncation_point = 12) {
  assertthat::assert_that(
    array_dim == round(array_dim),
    array_dim >= 3,
    is.numeric(sigma),
    truncation_point > 0
  )
  limit <- floor(sigma * 12)
  template <- numeric(array_dim)
  template[1] <- dnorm(0, mean = 0, sd = sigma)
  seq <- seq(from = 1, to = limit)
  salience <- dnorm(seq, mean = 0, sd = sigma)
  template[2:(limit + 1)] <- salience
  template[array_dim:(array_dim - limit + 1)] <- salience
  template
}

make_gaussian_spectrum <- function(
  array_dim, mean, mass, sigma, truncation_point = 12
) {
  assertthat::assert_that(
    array_dim == round(array_dim),
    array_dim > 0,
    mean >= 0,
    mean <= array_dim
  )
  origin <- round(mean)
  template <- make_gaussian_spectral_template(
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
make_complex_tone <- function(
  fundamental_pc,
  array_dim,
  num_harmonics,
  rho,
  sigma
) {
  assertthat::assert_that(
    is.numeric(fundamental_pc),
    assertthat::is.scalar(fundamental_pc),
    fundamental_pc < 12,
    is.numeric(array_dim), assertthat::is.scalar(array_dim),
    is.numeric(num_harmonics), assertthat::is.scalar(num_harmonics),
    num_harmonics >= 0,
    is.numeric(rho), assertthat::is.scalar(rho),
    is.numeric(sigma), assertthat::is.scalar(sigma),
    sigma >= 0
  )
  pcs <- vapply(seq_len(num_harmonics),
                function(i) {
                  (
                    (fundamental_pc * array_dim / 12) +
                      (array_dim * log(i, 2))
                  ) %% array_dim
                }, numeric(1))
  saliences <- vapply(seq_len(num_harmonics),
                      function(i) {
                        1 / (i ^ rho)
                      }, numeric(1))
  spectra <- mapply(
    function(pc, salience) {
      make_gaussian_spectrum(array_dim, pc, salience, sigma)
    }, pcs, saliences, SIMPLIFY = TRUE
  )
  spectrum <- rowSums(spectra)
  spectrum
}

#' Get complex tone
#'
#' Wrapper for \code{make_complex_tone} that implements caching.
#' @export
get_complex_tone <- memoise::memoise(make_complex_tone)

#' @export
get_cosine_similarity <- function(x, y) {
  numerator <- sum(x * y)
  denominator <-
    sqrt(sum(x ^ 2)) *
    sqrt(sum(y ^ 2))
  numerator / denominator
}


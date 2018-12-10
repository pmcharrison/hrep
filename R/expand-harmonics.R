#' Expand harmonics
#'
#' Expands each tone in an object into its implied harmonics.
#' @param x Object whose harmonics should be expanded.
#' @param ... Further parameters to be passed to
#' \code{\link{expand_harmonics.fr_sparse_spectrum}}.
#' @export
expand_harmonics <- function(x, ...) {
  UseMethod("expand_harmonics")
}

#' Expand harmonics
#'
#' Expands each tone in an object into its implied harmonics.
#' @param num_harmonics (Integerish scalar) Number of harmonics produced by each tone,
#' including the fundamental frequency.
#' @param roll_off (Numeric scalar) Parametrises the amount of amplitude roll-off
#' in the harmonics, with greater values corresponding to higher roll-off.
#' @param frequency_digits (Integerish scalar) Number of significant digits
#' to which frequencies are rounded before adjacent partials are combined.
#' @export
expand_harmonics.fr_sparse_spectrum <- function(x,
                                                num_harmonics = 11, # including the fundamental
                                                roll_off = 1,
                                                frequency_digits = 6,
                                                ...) {
  template <- tibble::tibble(n = seq_len(num_harmonics),
                             amplitude = 1 / (n ^ roll_off))
  purrr::map2(freq(x), amp(x),
              function(freq, amp) {
                tibble::tibble(
                  frequency = round(freq * template$n,
                                    digits = frequency_digits),
                  amplitude = amp * template$amplitude)
              }) %>%
    do.call(rbind, .) %>%
    {reduce_by_key(
      keys = .$frequency, values = .$amplitude,
      function(x, y) sum_amplitudes(x, y, coherent = FALSE),
      key_type = "numeric"
    )} %>%
    {.fr_sparse_spectrum(frequency = .$key, amplitude = .$value)}
}

#' @export
expand_harmonics.pi_sparse_spectrum <- function(x, ...) {
  fr_sparse_spectrum(x) %>%
    expand_harmonics(...) %>%
    pi_sparse_spectrum()
}

#' @export
expand_harmonics.pi_chord <- function(x, ...) {
  pi_sparse_spectrum(x, ...)
}

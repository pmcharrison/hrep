#' Expand harmonics
#'
#' Expands each tone in an object into its implied harmonics.
#' @param x Object whose harmonics should be expanded.
#' @param ... Further parameters to be passed to individual methods.
#' @rdname expand_harmonics
#' @export
expand_harmonics <- function(x, ...) {
  UseMethod("expand_harmonics")
}

#' @param num_harmonics (Integerish scalar) Number of harmonics produced by each tone,
#' including the fundamental frequency.
#' @param roll_off (Numeric scalar) Parametrises the amount of amplitude roll-off
#' in the harmonics, with greater values corresponding to higher roll-off.
#' @param frequency_digits (Integerish scalar) Number of significant digits
#' to which frequencies are rounded.
#' @rdname expand_harmonics
#' @export
expand_harmonics.fr_sparse_spectrum <- function(x,
                                                num_harmonics = 11L,
                                                roll_off = 1,
                                                frequency_digits = 6,
                                                ...) {
  template <- fr_harmonic_template(num_harmonics, roll_off)

  purrr::map2(freq(x), amp(x),
              function(freq, amp) {
                .fr_sparse_spectrum(
                  frequency = round(freq * template$n,
                                    digits = frequency_digits),
                  amplitude = amp * template$amplitude)
              }) %>%
    do.call(c, .)
}

#' @param round Whether or not the harmonic template should be rounded.
#' @rdname expand_harmonics
#' @export
expand_harmonics.pi_sparse_spectrum <- function(x,
                                                num_harmonics = 11L,  # including the fundamental
                                                roll_off = 1,
                                                round = FALSE,
                                                ...) {
  template <- pi_harmonic_template(num_harmonics, roll_off, round)

  purrr::map2(pitch(x), amp(x),
              function(pitch, amp) {
                .pi_sparse_spectrum(
                  pitch = pitch + template$interval,
                  amplitude = amp * template$amplitude)
              }) %>%
    do.call(c, .)
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.pi_chord <- function(x, ...) {
  pi_sparse_spectrum(x, ...)
}

fr_harmonic_template <- function(num_harmonics, roll_off) {
  tibble::tibble(n = seq_len(num_harmonics),
                 amplitude = 1 / (.data$n ^ roll_off))
}

pi_harmonic_template <- function(num_harmonics, roll_off, round = FALSE) {
  x <- tibble::tibble(n = seq_len(num_harmonics),
                      interval = 12 * log(.data$n, base = 2),
                      amplitude = 1 / (.data$n ^ roll_off))
  if (round) x$interval <- round(x$interval)
  x
}

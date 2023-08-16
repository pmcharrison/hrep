#' Expand harmonics
#'
#' Expands each tone in an object into its implied harmonics.
#'
#' @param x Object whose harmonics should be expanded.
#' Should be of class
#' \code{sparse_pi_spectrum},
#' \code{sparse_fr_spectrum}, or
#' \code{pi_chord}.
#'
#' @param num_harmonics
#' (Integerish scalar)
#' Number of harmonics (including the fundamental) to which
#' each tone should be expanded.
#'
#' @param roll_off (Numeric scalar) Parametrises the amount of amplitude roll-off
#' in the harmonics, with greater values corresponding to higher roll-off.
#'
#' @param digits
#' Number of digits to which each partial's MIDI pitch should be rounded.
#'
#' @param label_harmonics
#' If TRUE, then the harmonics in the resulting spectrum are labelled with their harmonic numbers.
#'
#' @param octave_ratio
#' The octave ratio for stretching and compressing harmonics, defaults to 2.0.
#'
#' @rdname expand_harmonics
#'
#' @inheritParams collapse_summing_amplitudes
#' @export
expand_harmonics <- function(x,
                             num_harmonics = 11L,
                             roll_off = 1,
                             digits = 6,
                             label_harmonics = FALSE,
                             coherent = FALSE,
                             octave_ratio = 2.0) {
  UseMethod("expand_harmonics")
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.sparse_fr_spectrum <- function(x,
                                                num_harmonics = 11L,
                                                roll_off = 1,
                                                digits = 6,
                                                label_harmonics = FALSE,
                                                coherent = FALSE,
                                                octave_ratio = 2.0) {
  expand_harmonics(sparse_pi_spectrum(x),
                   num_harmonics = num_harmonics,
                   roll_off = roll_off,
                   digits = digits,
                   label_harmonics = label_harmonics,
                   coherent = coherent) %>%
    sparse_fr_spectrum()
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.sparse_pi_spectrum <- function(x,
                                                num_harmonics = 11L,
                                                roll_off = 1,
                                                digits = 6,
                                                label_harmonics = FALSE,
                                                coherent = FALSE,
                                                octave_ratio = 2.0) {
  purrr::map2(pitch(x), amp(x),
              function(pitch, amp) {
                n  <- seq_len(num_harmonics)
                f0 <- midi_to_freq(pitch)
                df <- data.frame(
                  x = freq_to_midi(f0 * octave_ratio ^ log2(n)),
                  y = 1 * 10 ^ ( -roll_off * log2(n) / 20)
                )
                if (label_harmonics) df$labels <- seq_along(df$x)
                df
              }) %>%
    collapse_summing_amplitudes(digits = digits, coherent = coherent) %>%
    {.sparse_pi_spectrum(pitch = .$x, amplitude = .$y, labels = .$labels)}
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.pi_chord <- function(x,
                                      num_harmonics = 11L,
                                      roll_off = 1,
                                      digits = 6,
                                      label_harmonics = FALSE,
                                      coherent = FALSE,
                                      octave_ratio = 2.0) {
  sparse_pi_spectrum(x,
                     num_harmonics = num_harmonics,
                     roll_off = roll_off,
                     digits = digits,
                     label_harmonics = label_harmonics)
}

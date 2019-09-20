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
#' @rdname expand_harmonics
#' @export
expand_harmonics <- function(x,
                             num_harmonics = 11L,
                             roll_off = 1,
                             digits = 6) {
  UseMethod("expand_harmonics")
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.sparse_fr_spectrum <- function(x,
                                                num_harmonics = 11L,
                                                roll_off = 1,
                                                digits = 6) {
  expand_harmonics(sparse_pi_spectrum(x),
                   num_harmonics = num_harmonics,
                   roll_off = roll_off,
                   digits = digits) %>%
    sparse_fr_spectrum()
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.sparse_pi_spectrum <- function(x,
                                                num_harmonics = 11L,
                                                roll_off = 1,
                                                digits = 6) {
  template <- pi_harmonic_template(num_harmonics, roll_off)

  purrr::map2(pitch(x), amp(x),
              function(pitch, amp) {
                data.frame(
                  x = pitch + template$interval,
                  y = amp * template$amplitude
                )
              }) %>%
    collapse_summing_amplitudes(digits = digits) %>%
    {.sparse_pi_spectrum(pitch = .$x, amplitude = .$y)}
}

#' @rdname expand_harmonics
#' @export
expand_harmonics.pi_chord <- function(x,
                                      num_harmonics = 11L,
                                      roll_off = 1,
                                      digits = 6) {
  sparse_pi_spectrum(x,
                     num_harmonics = num_harmonics,
                     roll_off = roll_off,
                     digits = digits)
}

pi_harmonic_template <- function(num_harmonics, roll_off, digits = 6) {
  tibble::tibble(n = seq_len(num_harmonics),
                 interval = 12 * log(.data$n, base = 2),
                 amplitude = 1 / (.data$n ^ roll_off))
}

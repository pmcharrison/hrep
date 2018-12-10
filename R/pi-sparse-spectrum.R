#' @export
.pi_sparse_spectrum <- function(x, y, dB) {
  checkmate::qassert(x, "N")
  checkmate::qassert(y, "N")
  checkmate::qassert(dB, "B1")
  stopifnot(length(x) == length(y))

  res <- sparse_spectrum(x = x, y = y,
                         x_unit = "midi",
                         y_unit = if (dB) "dB" else "amplitude",
                         label = "pitch spectrum",
                         x_lab = "Pitch",
                         y_lab = if (dB) "Level (dB)" else "Amplitude")
  class(res) <- c("pi_sparse_spectrum", class(res))
  res
}

#' @export
pi_sparse_spectrum <- function(x, ...) {
  UseMethod("pi_sparse_spectrum")
}

#' @export
#' @param ... Further arguments passed to \code{\link{expand_harmonics}()}.
pi_sparse_spectrum.pi_chord <- function(x,
                                        dB = FALSE,
                                        amplitude = if (dB) 60 else 1,
                                        ...) {
  res <- expand_harmonics(frequency = as.numeric(x),
                          amplitude = amplitude,
                          dB = dB,
                          frequency_scale = "midi",
                          ...)
  .pi_sparse_spectrum(x = res$x, y = res$y, dB = dB)
}

#' @export
pi_sparse_spectrum.default <- function(x, ...) {
  pi_sparse_spectrum(pi_chord(x), ...)
}

#' Given a frequency spectrum, re-express the amplitudes of each partial
#' in decibels.
#' @param x Frequency spectrum
#' @param unit_amplitude_in_dB An amplitude of 1 should be mapped
#' to this decibel value.
#' @export
to_dB <- function(x, unit_amplitude_in_dB = 60, ...) {
  checkmate::qassert(unit_amplitude_in_dB, "N1")
  UseMethod("to_dB")
}

to_dB.pi_sparse_spectrum <- function(x, unit_amplitude_in_dB = 60, ...) {
  if (y_unit(x) == "dB") {
    return(x)
  } else if (y_unit(x) == "amplitude") {
    x$y <- amplitude_to_dB(x$y, unit_amplitude_in_dB = unit_amplitude_in_dB)
    y_unit(x) <- "dB"
    y_lab(x) <- "Level (dB)"
    return(x)
  } else stop("unrecognised value of 'y_unit': ", y_unit(x))
}

#' @param frequency (Numeric vector) Frequencies in the input spectrum.
#' @param amplitude (Numeric vector) Amplitudes in the input spectrum.
#' @param dB (Logical scalar)
#' Is \code{amplitude} provided on the decibel scale?
#' @param (Numeric scalar) frequency_digits
#' Round frequencies to this number of significant digits before
#' combining partials.
#' @return A data frame with a numeric column <frequency>
#' and a numeric column <amplitude>.
#' This data.frame describes the expanded harmonic spectrum.
#' Whether or not the \code{amplitude} column is given in dB corresponds
#' to whether or not the argument \code{dB} is \code{TRUE}.
#' @export
expand_harmonics <- function(
  frequency,
  amplitude,
  dB = FALSE,
  frequency_scale = "Hz",
  num_harmonics = 11, # including the fundamental
  roll_off = 1,
  frequency_digits = 6
) {
  amplitude <- rep_to_match(amplitude, frequency)
  expand_harmonics_check_inputs(frequency, amplitude, num_harmonics, roll_off)
  if (dB) {
    # Modify the call, converting amplitude from dB, and redo it
    unit_amplitude_in_dB <- 60
    expand_harmonics(
      frequency = frequency,
      amplitude = dB_to_amplitude(amplitude, unit_amplitude_in_dB),
      dB = FALSE,
      frequency_scale = frequency_scale,
      num_harmonics = num_harmonics,
      roll_off = roll_off,
      frequency_digits = frequency_digits
    ) %>% to_dB
  } else {
    template <- get_harmonic_template(
      num_harmonics, 1, roll_off,
      interval_scale = if (frequency_scale == "Hz") "ratio" else "midi"
    )
    mapply(
      FUN = function(fundamental_frequency, fundamental_amplitude) {
        data.frame(
          frequency = add_interval(fundamental_frequency,
                                   template$interval,
                                   frequency_scale = frequency_scale),
          amplitude = fundamental_amplitude * template$amplitude
        )
      }, frequency, amplitude,
      SIMPLIFY = FALSE
    ) %>% do.call(rbind, .) %>% {
      reduce_by_key(
        keys = .$frequency,
        values = .$amplitude,
        function(x, y) sum_amplitudes(x, y, coherent = FALSE),
        key_type = "numeric"
      )} %>%
      {.pi_sparse_spectrum(x = .$key, y = .$value, dB = FALSE)}
  }
}

expand_harmonics_check_inputs <- function(
  frequency, amplitude, num_harmonics, roll_off
) {
  stopifnot(length(frequency) == length(amplitude))
  checkmate::qassert(num_harmonics, "N1")
  checkmate::qassert(roll_off, "N1")
}

#' Get harmonic template
#'
#' Gets a harmonic template, using non-stretched octaves.
#' @param num_harmonics Number of harmonics (including fundamental)
#' @param amplitude amplitude of the fundamental frequency
#' @param roll_off Roll-off constant
#' @param interval_scale Can be "ratio" or "midi"
#' @return \code{data.frame} of frequencies and amplitudes
#' @export
get_harmonic_template <- function(
  num_harmonics,
  amplitude,
  roll_off = get_midi_params()$roll_off,
  interval_scale = "ratio",
  round_midi_intervals = TRUE
) {
  stopifnot(interval_scale %in% c("ratio", "midi"))
  harmonic_numbers <- seq(from = 0, length.out = num_harmonics)
  intervals <- if (interval_scale == "ratio") {
    harmonic_numbers + 1
  } else {
    (12 * log(harmonic_numbers + 1, base = 2)) %>% (function(x) {
      if (round_midi_intervals) round(x) else x
    })
  }
  attr(intervals, "scale") <- interval_scale
  amplitudes <- amplitude / ((1 + harmonic_numbers) ^ roll_off)
  attr(amplitudes, "scale") <- "not dB"
  data.frame(
    interval = intervals,
    amplitude = amplitudes
  )
}

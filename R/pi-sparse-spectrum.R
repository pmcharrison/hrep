#' @export
pi_sparse_spectrum <- function(x, dB = FALSE, amplitude = if (dB) 60 else 1, ...) {
  stopifnot(is.pi_chord(x))
  expand_harmonics(frequency = as.numeric(x),
                   amplitude = amplitude,
                   dB = dB,
                   frequency_scale = "midi",
                   ...)
}

#' @param frequency Numeric vector of frequencies
#' @param amplitude Numeric vector of amplitudes
#' @param dB Logical, determines whether \code{amplitude} is assumed to be in dB or not. This matters becuse the 1 / n amplitude roll-off rule assumes NOT dB.
#' @param frequency_digits Frequencies are rounded to this number of significant digits before combination.
#' @return a \code{data.frame} with a numeric column <frequency> and a numeric column <amplitude>. This data.frame describes the expanded harmonic spectrum. Whether or not the \code{amplitude} column is given in dB corresponds to whether or not the argument \code{dB} is \code{TRUE}.
#' @export
expand_harmonics <- function(
  frequency,
  amplitude,
  dB = FALSE,
  frequency_scale = "Hz",
  num_harmonics = get_midi_params()$num_harmonics, # including the fundamental
  roll_off = get_midi_params()$roll_off,
  frequency_digits = get_midi_params()$frequency_digits
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
    ) %>%
      transform_y(function(x) amplitude_to_dB(x, unit_amplitude_in_dB),
                  y_unit = "dB", "Level (dB)")
  } else {
    template <- get_harmonic_template(
      num_harmonics, 1, roll_off,
      interval_scale = if (frequency_scale == "Hz") "ratio" else "midi"
    )
    res <- mapply(
      FUN = function(fundamental_frequency, fundamental_amplitude) {
        data.frame(
          frequency = add_interval(fundamental_frequency,
                                   template$interval,
                                   frequency_scale = frequency_scale),
          amplitude = fundamental_amplitude * template$amplitude
        )
      }, frequency, amplitude,
      SIMPLIFY = FALSE
    ) %>% (function(x) do.call(rbind, x)) %>%
      (function (df) {
        reduce_by_key(
          keys = df$frequency,
          values = df$amplitude,
          function(x, y) sum_amplitudes(x, y, coherent = FALSE),
          key_type = "numeric"
        )
      }) %>% rename_columns(c(key = "x", value = "y"))
    sparse_spectrum(x = res$x, y = res$y,
                    x_unit = "midi",
                    y_unit = if (dB) "dB" else "amplitude",
                    label = "pitch spectrum",
                    x_lab = "Pitch",
                    y_lab = if (dB) "level (dB)" else "Amplitude")
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

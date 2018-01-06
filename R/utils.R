#' Convert MIDI note numbers to frequencies
#'
#' Converts MIDI note numbers to frequencies (kHz), optionally using stretched octaves. Corresponds to Equation 1 of Parncutt & Strasburger (1994).
#' @param midi Numeric vector of MIDI note numbers
#' @param stretched_octave Logical scalar; whether or not to use a stretched octave. Default is \code{FALSE}
#' @param tuning_ref_Hz The tuning reference point in Hz, should correspond to the frequency of the A above middle C (typically 440 Hz)
#' @return Numeric vector of frequencies in Hz
#' @export
convert_midi_to_freq <- function(
  midi,
  stretched_octave = get_midi_params()$stretched_octave,
  tuning_ref_Hz = get_midi_params()$tuning_ref_Hz
) {
  assertthat::assert_that(
    is.numeric(midi),
    is.logical(stretched_octave), assertthat::is.scalar(stretched_octave),
    is.numeric(tuning_ref_Hz), assertthat::is.scalar(tuning_ref_Hz)
  )
  tuning_ref_Hz * (2 ^ ((midi - 69) / if (stretched_octave) 11.9 else 12))
}

#' @export
convert_amplitude_to_dB <- function(
  amplitude,
  unit_amplitude_in_dB
) {
  assertthat::assert_that(assertthat::is.scalar(unit_amplitude_in_dB))
  amplitude_ref = 10 ^ (- unit_amplitude_in_dB / 20)
  20 * log10(amplitude / amplitude_ref)
}

#' @export
convert_dB_to_amplitude <- function(
  dB,
  unit_amplitude_in_dB
) {
  assertthat::assert_that(assertthat::is.scalar(unit_amplitude_in_dB))
  amplitude_ref = 10 ^ (- unit_amplitude_in_dB / 20)
  amplitude_ref * 10 ^ (dB / 20)
}

#' Get harmonic template
#'
#' Gets a harmonic template, using non-stretched octaves.
#' @param num_harmonics Number of harmonics (including fundamental)
#' @param amplitude amplitude of the fundamental frequency
#' @return \code{data.frame} of frequencies and amplitudes
#' @export
get_harmonic_template <- function(
  num_harmonics,
  amplitude,
  roll_off = get_midi_params()$roll_off
) {
  harmonic_numbers <- seq(from = 0, length.out = num_harmonics)
  template <- data.frame(frequency_ratio = harmonic_numbers + 1,
                         amplitude = amplitude / ((1 + harmonic_numbers) ^ roll_off))
  template
}

#' @param frequency Numeric vector of frequencies
#' @param amplitude Numeric vector of amplitudes
#' @param frequency_digits Frequencies are rounded to this number of significant digits before combination.
#' @return a \code{data.frame} with a numeric column <frequency> and a numeric column <amplitude>. This data.frame describes the expanded harmonic spectrum.
#' @export
expand_harmonics <- function(
  frequency,
  amplitude,
  num_harmonics = get_midi_params()$num_harmonics, # including the fundamental
  roll_off = get_midi_params()$roll_off,
  frequency_digits = get_midi_params()$frequency_digits
) {
  amplitude <- if (length(amplitude) == 1) rep(amplitude, times = length(frequency)) else amplitude
  assertthat::assert_that(
    length(frequency) == length(amplitude),
    is.numeric(num_harmonics), assertthat::is.scalar(num_harmonics),
    is.numeric(roll_off), assertthat::is.scalar(roll_off)
  )
  template <- get_harmonic_template(
    num_harmonics = num_harmonics,
    amplitude = 1,
    roll_off = roll_off
  )
  spectrum <- new.env()
  for (i in seq_along(frequency)) {
    # Iterate over every fundamental frequency and add the spectral template
    fundamental_frequency <- frequency[i]
    fundamental_amplitude <- amplitude[i]
    mapply(function(frequency, amplitude) {
      key <- format(frequency, digits = frequency_digits, scientific = TRUE)
      spectrum[[key]] <<- if (is.null(spectrum[[key]])) amplitude else {
        sum_amplitudes(spectrum[[key]], amplitude, coherent = FALSE)
      }
    },
    template$frequency_ratio * fundamental_frequency,
    template$amplitude * fundamental_amplitude)
  }
  spectrum <- as.list(spectrum) %>%
    (function(x) data.frame(
      frequency = as.numeric(names(x)),
      amplitude = as.numeric(unlist(x)))) %>%
    (function(df) {
      df <- df[order(df$frequency), ]
      rownames(df) <- NULL
      df
    })
  spectrum
}

#' Sum amplitudes
#'
#' Sums amplitudes for pairs of pure tones (can be vectorised).
#' @param x First amplitude to sum (numeric vector)
#' @param y Second amplitude to sum (numeric vector)
#' @param coherent Whether or not the phases of the two tones are coherent (logical scalar)
#' @export
sum_amplitudes <- function(x, y, coherent = FALSE) {
  assertthat::assert_that(
    is.numeric(x), assertthat::is.scalar(x),
    is.numeric(y), assertthat::is.scalar(y),
    is.logical(coherent), assertthat::is.scalar(coherent)
  )
  if (coherent) {
    x + y
  } else {
    sqrt(x ^ 2 + y ^ 2)
  }
}

#' Note: this could be done more efficiently with ifft
#' @export
convert_sparse_spectrum_to_waveform <- function(
  frequency,
  amplitude,
  seconds = 1,
  sample_rate = 44e3,
  bit = 16
) {
  x <- seq(from = 0, to = seconds, length.out = sample_rate * seconds)
  y <- mapply(
    function(freq, amplitude) {
      sin(2 * pi * freq * x) * amplitude
    },
    frequency,
    amplitude
  ) %>% rowSums %>%
    (function (y) y / max(abs(y))) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  data.frame(t = x, y = y)
}

#' @export
play_sparse_spectrum <- function(
  frequency, amplitude,
  sample_rate = 44e3,
  seconds = 1,
  bit = 16
) {
  spectrum <- convert_sparse_spectrum_to_waveform(
    frequency = frequency, amplitude = amplitude,
    second = seconds, sample_rate = sample_rate, bit = bit
  )
  tuneR::play(tuneR::Wave(spectrum$y, samp.rate = sample_rate, bit = bit),
              "play")
}

#' @export
plot_sparse_spectrum <- function(
  frequency, amplitude,
  resolution_Hz = 1,
  range_Hz = NULL,
  theme = ggplot2::theme_bw()
) {
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  df <- data.frame(freq = round(frequency),
                   amp = amplitude) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ]) %>%
    rbind(
      data.frame(freq = seq(from = range_Hz[1],
                            to = range_Hz[2],
                            by = resolution_Hz),
                 amp = 0)
    ) %>%
    (function(df) {
      df[order(df$freq), ]
    })
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme
  print(p)
}

#' @export
plot_spectrum <- function(
  frequency,
  amplitude,
  range_Hz = NULL,
  theme = ggplot2::theme_bw()
) {
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  order <- order(frequency)
  df <- data.frame(freq = frequency[order], amp = amplitude[order]) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ])
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme
  print(p)
}

#' @export
plot_waveform <- function(
  x,
  sample_rate = 44000,
  range_sec = c(0, 0.2),
  theme = ggplot2::theme_bw()
) {
  df <- data.frame(
    t = seq(from = 0, by = 1 / sample_rate, length.out = length(x)),
    x = x
  ) %>%
    (function(df) df[df$t >= range_sec[1] & df$t <= range_sec[2], ])
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "t", y = "x")) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_x_continuous("Time (sec)") +
    ggplot2::scale_y_continuous("Instantaneous amplitude") +
    theme
  print(p)
}

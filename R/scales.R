#' Convert pitches to pitch classes
#'
#' Converts pitches to pitch classes.
#' @param x A numeric vector of pitches
#' @return A numeric vector produced by mapping each element in \code{pitch} to a pitch class
#' @export
pi_to_pc <- function(x) {
  x %% 12
}

#' Get the distance between two pitch classes.
#' @param x Vector of pitch classes
#' @param y Vector of pitch classes
#' @export
pc_dist <- function(x, y) {
  pmin(
    abs(x - y),
    12 - abs(x - y)
  )
}

#' Get the ascending distance between two pitch classes.
#' @param x Vector of pitch classes
#' @param y Vector of pitch classes
#' @export
ascending_pc_dist <- function(x, y) {
  ifelse(x <= y, y - x, y + 12L - x)
}

#' Convert MIDI note numbers to frequencies
#'
#' Converts MIDI note numbers to frequencies (Hz), optionally using stretched octaves. Corresponds to Equation 1 of Parncutt & Strasburger (1994), but with Hz instead of kHz.
#' @param midi Numeric vector of MIDI note numbers
#' @param stretched_octave Logical scalar; whether or not to use a stretched octave. Default is \code{FALSE}
#' @param tuning_ref_Hz The tuning reference point in Hz, should correspond to the frequency of the A above middle C (typically 440 Hz)
#' @return Numeric vector of frequencies in Hz
#' @export
midi_to_freq <- function(
  midi,
  stretched_octave = get_midi_params()$stretched_octave,
  tuning_ref_Hz = get_midi_params()$tuning_ref_Hz
) {
  checkmate::qassert(midi, "N")
  checkmate::qassert(stretched_octave, "B1")
  checkmate::qassert(tuning_ref_Hz, "N1(0,)")
  tuning_ref_Hz * (2 ^ ((midi - 69) / if (stretched_octave) 11.9 else 12))
}

#' Convert frequencies to MIDI note numbers
#'
#' Converts frequencies (Hz) to MIDI note numbers, optionally using stretched octaves. Non-integer MIDI note numbers are permitted as output.
#' @param midi Numeric vector of MIDI note numbers
#' @param stretched_octave Logical scalar; whether or not to use a stretched octave. Default is \code{FALSE}
#' @param tuning_ref_Hz The tuning reference point in Hz, should correspond to the frequency of the A above middle C (typically 440 Hz)
#' @return Numeric vector of frequencies in Hz
#' @export
freq_to_midi <- function(
  frequency,
  stretched_octave = get_midi_params()$stretched_octave,
  tuning_ref_Hz = get_midi_params()$tuning_ref_Hz
) {
  checkmate::qassert(frequency, "N(0,)")
  checkmate::qassert(stretched_octave, "B1")
  checkmate::qassert(tuning_ref_Hz, "N1(0,)")
  69 + log(frequency / tuning_ref_Hz, base = 2) * if (stretched_octave) 11.9 else 12
}

#' @export
amplitude_to_dB <- function(
  amplitude,
  unit_amplitude_in_dB
) {
  checkmate::qassert(unit_amplitude_in_dB, "N1")
  amplitude_ref = 10 ^ (- unit_amplitude_in_dB / 20)
  res <- 20 * log10(amplitude / amplitude_ref)
  attr(res, "units") <- "dB"
  res
}

#' @export
dB_to_amplitude <- function(
  dB,
  unit_amplitude_in_dB
) {
  checkmate::qassert(unit_amplitude_in_dB, "N1")
  amplitude_ref = 10 ^ (- unit_amplitude_in_dB / 20)
  res <- amplitude_ref * 10 ^ (dB / 20)
  attr(res, "units") <- "not dB"
  res
}

add_interval <- function(frequency, interval, frequency_scale) {
  checkmate::qassert(frequency_scale, "S1")
  stopifnot(frequency_scale %in% c("Hz", "midi"))
  if (frequency_scale == "Hz") {
    frequency * interval
  } else {
    frequency + interval
  }
}

#' Sum amplitudes
#'
#' Sums amplitudes for pairs of pure tones (can be vectorised).
#' @param x First amplitude to sum (numeric vector)
#' @param y Second amplitude to sum (numeric vector)
#' @param coherent Whether or not the phases of the two tones are coherent (logical scalar)
#' @param dB Whether or not the amplitudes are provided in decibels (dB)
#' @export
sum_amplitudes <- function(x, y, coherent = FALSE, dB = FALSE) {
  checkmate::qassert(x, "N")
  checkmate::qassert(y, "N")
  checkmate::qassert(coherent, "B1")
  checkmate::qassert(dB, "B1")
  stopifnot(length(x) == length(y))
  if (dB) {
    if (coherent) {
      20 * log10(10 ^ (x / 20) + 10 ^ (y / 20))
    } else {
      10 * log10(10 ^ (x / 10) + 10 ^ (y / 10))
    }
  } else {
    if (coherent) {
      x + y
    } else {
      sqrt(x ^ 2 + y ^ 2)
    }
  }
}

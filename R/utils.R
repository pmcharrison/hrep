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
  res <- 20 * log10(amplitude / amplitude_ref)
  attr(res, "units") <- "dB"
  res
}

#' @export
convert_dB_to_amplitude <- function(
  dB,
  unit_amplitude_in_dB
) {
  assertthat::assert_that(assertthat::is.scalar(unit_amplitude_in_dB))
  amplitude_ref = 10 ^ (- unit_amplitude_in_dB / 20)
  res <- amplitude_ref * 10 ^ (dB / 20)
  attr(res, "units") <- "not dB"
  res
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
  assertthat::assert_that(
    interval_scale %in% c("ratio", "midi")
  )
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
      amplitude = convert_dB_to_amplitude(amplitude, unit_amplitude_in_dB),
      dB = FALSE,
      frequency_scale = frequency_scale,
      num_harmonics = num_harmonics,
      roll_off = roll_off,
      frequency_digits = frequency_digits
    ) %>%
      (function(df) {
        df$amplitude <- convert_amplitude_to_dB(df$amplitude, unit_amplitude_in_dB)
        df
      })
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
    ) %>% do.call(rbind, .) %>%
      (function (df) {
        reduce_by_key(
          keys = df$frequency,
          values = df$amplitude,
          function(x, y) sum_amplitudes(x, y, coherent = FALSE),
          key_type = "numeric"
        )
      }) %>% rename_columns(c(key = "frequency", value = "amplitude")) %>%
      add_attributes(
        spec <- list(
          frequency = list(scale = frequency_scale),
          amplitude = list(scale = "not dB")
        )
      )
  }
}

expand_harmonics_check_inputs <- function(
  frequency, amplitude, num_harmonics, roll_off
) {
  assertthat::assert_that(
    length(frequency) == length(amplitude),
    is.numeric(num_harmonics), assertthat::is.scalar(num_harmonics),
    is.numeric(roll_off), assertthat::is.scalar(roll_off)
  )
}

add_interval <- function(frequency, interval, frequency_scale) {
  assertthat::assert_that(
    assertthat::is.scalar(frequency_scale),
    frequency_scale %in% c("Hz", "midi")
  )
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
  assertthat::assert_that(
    is.numeric(x), assertthat::is.scalar(x),
    is.numeric(y), assertthat::is.scalar(y),
    is.logical(coherent), assertthat::is.scalar(coherent),
    length(x) == length(y)
  )
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
play_sparse_spectrum <- function( # nocov start
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
} # nocov end

#' @export
plot_sparse_spectrum <- function( # nocov start
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
} # nocov end

#' @export
plot_spectrum <- function( # nocov start
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
} # nocov end

#' @export
plot_waveform <- function( # nocov start
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
} # nocov end

reduce_by_key <- function(keys, values, f, key_type = "character") {
  assertthat::assert_that(
    length(keys) == length(values),
    is.function(f)
  )
  keys <- as.character(keys)
  env <- new.env()
  n <- length(keys)
  for (i in seq_len(n)) {
    key <- keys[i]
    value <- values[i]
    env[[key]] <- if (is.null(env[[key]])) {
      value
    } else {
      do.call(f, list(env[[key]], value))
    }
  }
  convert_env_to_df(env, key_type = key_type)
}

convert_env_to_df <- function(env, sort_by_key = TRUE, decreasing = FALSE, key_type = "character") {
  assertthat::assert_that(
    is.environment(env)
  )
  as.list(env) %>%
    (function(x) {
      data.frame(
        key = names(x) %>% as(key_type),
        value = unlist(x),
        stringsAsFactors = FALSE
      )
    }) %>%
    (function(df) {
      if (sort_by_key) {
        df[order(df$key, decreasing = decreasing), ]
      } else df
    }) %>% remove_row_names
}

remove_row_names <- function(df) {
  rownames(df) <- NULL
  df
}

rename_columns <- function(df, replace, warn_missing = TRUE) {
  names(df) <- plyr::revalue(
    names(df), replace = replace, warn_missing = warn_missing
  )
  df
}

add_attributes <- function(df, spec) {
  col_names <- names(spec)
  assertthat::assert_that(
    all(col_names %in% names(df))
  )
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    attributes <- spec[[i]]
    for (j in seq_along(attributes)) {
      attr_name <- names(attributes)[j]
      attr <- attributes[[j]]
      attr(df[[col_name]], attr_name) <- attr
    }
  }
  df
}

#' @export
rep_to_match <- function(x, y) {
  if (length(x) == 1) {
    rep(x, times = length(y))
  } else if (length(x) != length(y)) {
    stop("<x> must either have length 1 or the same length as <y>.")
  } else {
    x
  }
}

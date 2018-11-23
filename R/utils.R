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
      (function(df) {
        df$amplitude <- amplitude_to_dB(df$amplitude, unit_amplitude_in_dB)
        df
      })
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
      }) %>% rename_columns(c(key = "frequency", value = "amplitude")) %>%
      add_attributes(
        spec = list(
          frequency = list(scale = frequency_scale),
          amplitude = list(scale = "not dB")
        )
      )
    class(res) <- c("spectrum", class(res))
    res
  }
}

expand_harmonics_check_inputs <- function(
  frequency, amplitude, num_harmonics, roll_off
) {
  stopifnot(length(frequency) == length(amplitude))
  checkmate::qassert(num_harmonics, "N1")
  checkmate::qassert(roll_off, "N1")
}

#' Note: this could be done more efficiently with ifft
#' @export
sparse_spectrum_to_waveform <- function(
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

reduce_by_key <- function(keys, values, f, key_type = "character") {
  stopifnot(
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
  env_to_df(env, key_type = key_type)
}

env_to_df <- function(env, sort_by_key = TRUE, decreasing = FALSE, key_type = "character") {
  stopifnot(
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

#' @export
rename_columns <- function(df, replace, warn_missing = TRUE) {
  names(df) <- plyr::revalue(
    names(df), replace = replace, warn_missing = warn_missing
  )
  df
}

#' @export
add_attributes <- function(df, spec) {
  col_names <- names(spec)
  stopifnot(
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

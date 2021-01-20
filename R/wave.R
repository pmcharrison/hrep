#' wave constructor
#'
#' Constructor function for objects of class "wave".
#'
#' @param x Numeric vector of displacement values.
#' @param sample_rate (Numeric scalar) Sample rate.
#'
#' @export
.wave <- function(x, sample_rate) {
  checkmate::qassert(x, "N")
  checkmate::qassert(sample_rate, "X1")
  x <- as.numeric(x)
  class(x) <- c("wave", class(x))
  sample_rate(x) <- sample_rate
  x
}

#' @export
print.wave <- function(x, ...) {
  cat(
    "Wave object\n",
    "  Sample rate = ", sample_rate(x), "\n",
    "  Number of samples = ", length(x), "\n",
    "  Duration = ", length(x) / sample_rate(x), " seconds\n",
    sep = ""
  )
}

#' Wave
#'
#' This function represents an object as class "wave".
#' Under the hood, "wave" objects are numeric vectors
#' describing displacement as a function of time.
#' The sample rate can be accessed using the \code{\link{sample_rate}} accessor.
#'
#' @param x Input object.
#'
#' @inheritDotParams expand_harmonics
#'
#' @rdname wave
#' @export
wave <- function(x, ...) {
  UseMethod("wave")
}

#' @param length_sec (Numeric scalar) Length of the output wave, in seconds.
#'
#' @param sample_rate (Integerish scalar) The desired sample rate.
#'
#' @param rise_length
#' (Numeric scalar)
#' Chord fade-in time (seconds).
#'
#' @param fade_length
#' (Numeric scalar)
#' Chord fade-out time (seconds).
#'
#' @rdname wave
#' @export
wave.default <- function(x,
                         length_sec = 1,
                         sample_rate = 44100,
                         rise_length = 0,
                         fade_length = 0,
                         ...) {
  x <- sparse_fr_spectrum(x, ...)
  wave(x,
       length_sec = length_sec,
       rise_length = rise_length,
       fade_length = fade_length,
       sample_rate = sample_rate)
}

#' @export
wave.wave <- function(x, ...) x


#' @param phase
#' (Numeric scalar)
#' Single number specifying the phase of the underlying sine waves to be used.
#'
#' @rdname wave
#' @export
wave.sparse_fr_spectrum <- function(
  x,
  length_sec = 1,
  sample_rate = 44100,
  rise_length = 0,
  fade_length = 0,
  phase = 0,
  ...
) {
  checkmate::qtest(length_sec, "N1[0)")
  checkmate::qtest(sample_rate, "X1[1)")
  checkmate::qtest(phase, "N1")
  stopifnot(rise_length <= length_sec,
            fade_length <= length_sec)
  frequency <- freq(x)
  amplitude <- amp(x)
  num_samples <- sample_rate * length_sec
  time <- seq(from = 0,
              to = length_sec,
              length.out = num_samples + 1)[- (num_samples + 1)]
  wave <- mapply(
    function(frequency, amplitude) {
      amplitude * sin(2 * pi * frequency * time + phase)
    }, frequency, amplitude
  ) %>%
    rowSums() %>%
    .wave(sample_rate) %>%
    add_fades(rise_length, fade_length, sample_rate, num_samples)
}

add_fades <- function(wave, rise_length, fade_length, sample_rate, num_samples) {
  if (rise_length != 0) {
    rise_n_samples <- round(rise_length * sample_rate)
    stopifnot(rise_n_samples <= num_samples)
    if (rise_n_samples > 0) {
      ind <- 1:rise_n_samples
      wave[ind] <- wave[ind] * seq(from = 0, to = 1, length.out = rise_n_samples)
    }
  }
  if (fade_length != 0) {
    fade_n_samples <- round(fade_length * sample_rate)
    stopifnot(fade_n_samples <= num_samples)
    if (fade_n_samples > 0) {
      ind <- seq(to = num_samples, length.out = fade_n_samples)
      wave[ind] <- wave[ind] * seq(from = 1, to = 0, length.out = fade_n_samples)
    }
  }
  wave
}

#' @export
plot.wave <- function(x, ggplot = FALSE, xlab = "Time (seconds)", ylab = "Displacement",
                      ylim = NULL, ...) {
  df <- as.data.frame(x)
  if (ggplot) {
    assert_installed("ggplot2")
    df %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "time", y = "displacement")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(xlab, limits = c(0, df$time[nrow(df)])) +
      ggplot2::scale_y_continuous(ylab, limits = ylim)
  } else {
    plot(df$time, df$displacement, xlab = xlab, ylab = ylab, type = "l", ylim = ylim)
  }
}

#' Save wav file
#'
#' Saves object to a wav file by converting to the \code{\link{wave}} representation
#' and then writing to a wav file.
#'
#' @param x Object to save; currently only individual chords are supported.
#' Chords are coerced to a \code{\link{wave}} representation.
#'
#' @param file (Character scalar) Output file.
#'
#' @param amplitude
#' (Numeric scalar)
#' The wave is multiplied by this number before exporting.
#' The resulting wave should fall completely within the range [-1, 1].
#'
#' @param bit_depth
#' (Integer scalar)
#' The bit depth of the exported audio.
#'
#' @param end_pad
#' (Numeric scalar)
#' Duration of silence (seconds) appended to the end of the audio file,
#' used to avoid clicks and other artifacts.
#'
#' @inheritParams wave
#' @inheritDotParams expand_harmonics
#'
#' @seealso \code{\link{save_wav_sox}}
#'
#' @rdname save_wav
#' @export
save_wav <- function(
  x,
  file,
  amplitude = 0.1,
  bit_depth = 16L,
  length_sec = 1,
  fade_length = 0.1,
  rise_length = 0.1,
  end_pad = 0.05,
  ...
) {
  UseMethod("save_wav")
}

#' @export
save_wav.default <- function(
  x,
  file,
  amplitude = 0.1,
  bit_depth = 16,
  length_sec = 1,
  fade_length = 0.1,
  rise_length = 0.1,
  end_pad = 0.05,
  ...
) {
  wave <- wave(
    x,
    length_sec = length_sec,
    fade_length = fade_length,
    rise_length = rise_length,
    ...
  )
  scale <- 2 ^ (bit_depth - 1)
  vector <- round(as.numeric(wave) * amplitude * scale)
  if (any(vector < - scale | vector >= scale)) {
    stop(sprintf("the wave's maximum value was approximately %.2f times too big for exporting, ",
                 max(abs(vector / scale))),
         "consider reducing amplitude by at least this value")
  }
  vector <- c(vector, rep(0, times = round(end_pad * sample_rate(wave))))
  wave_2 <- tuneR::Wave(
    left = vector,
    right = vector,
    samp.rate = sample_rate(wave),
    bit = bit_depth
  )
  tuneR::writeWave(wave_2, file)
}

#' Play wav
#'
#' Plays a chord as a wave file.
#'
#' @param x Chord to save.
#'
#' @inheritParams tuneR::play
#' @inheritDotParams save_wav
#' @inheritDotParams expand_harmonics
#'
#' @seealso \code{\link{play_sox}}
#'
#' @export
play_wav <- function(x, player = "play", ...) {
  file <- tempfile(fileext = ".wav")
  save_wav(x, file, ...)
  tuneR::play(file, player = player)
  invisible(file.remove(file))
}

#' @export
as.data.frame.wave <- function(x, ...) {
  time <- seq(from = 0, by = 1 / sample_rate(x), length.out = length(x))
  data.frame(time = time, displacement = as.numeric(x))
}

#' Pad
#'
#' Adds silent padding to the beginning/end of a sound.
#'
#' @param before
#' (Numeric scalar)
#' Seconds of silence to add before the sound.
#'
#' @param after
#' (Numeric scalar)
#' Seconds of silence to add after the sound.
#'
#' @export
pad <- function(x, before = 0, after = 0) {
  UseMethod("pad")
}

#' @export
pad.wave <- function(x, before = 0, after = 0) {
  fs <- sample_rate(x)
  raw <- as.numeric(x)
  .wave(
    c(
      rep(0, times = round(before * fs)),
      raw,
      rep(0, times = round(after * fs))
    ),
    sample_rate = fs
  )
}

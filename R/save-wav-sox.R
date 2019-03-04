#' Save wav file (sox)
#'
#' Saves object to a wav file using the 'sox' timbre from sox
#' (\url{http://sox.sourceforge.net/}).
#'
#' @note
#' The command-line sound-processing program sox
#' (\url{http://sox.sourceforge.net/})
#' must be installed and available on the command line
#' under the command \code{sox}.
#'
#' @param x Object to save; methods exist for individual chords
#' and for vectors of chords (see \code{\link{vec}}).
#' Chords are coerced to a \code{\link{pi_chord}} representation.
#' @param file (Character scalar) Output file.
#' @param chord_length (Numeric scalar) Length of each output chord, in seconds.
#' @param ... Parameters passed to methods.
#'
#' @export
save_wav_sox <- function(x, file, ...) {
  UseMethod("save_wav_sox")
}

#' @export
save_wav_sox.default <- function(x, ...) {
  save_wav_sox(pi_chord(x), ...)
}

#' @export
save_wav_sox.coded_vec <- function(x, file, ...) {
  save_wav_sox(decode(x), file, ...)
}

#' @export
save_wav_sox.pi_chord <- function(x, ...) {
  save_wav_sox(fr_chord(x), ...)
}

#' @export
save_wav_sox.sparse_pi_spectrum <- function(x, ...) {
  save_wav_sox(sparse_fr_spectrum(x), ...)
}

#' @export
save_wav_sox.fr_chord <- function(x, file, timbre = "pluck", ...) {
  save_wav_sox(
    .sparse_fr_spectrum(frequency = as.numeric(x),
                        amplitude = rep(1, times = length(x))),
    file = file,
    timbre = timbre,
    ...
  )
}

#' @export
save_wav_sox.vec <- function(x, file, reverb = 20, ...) {
  files <- paste("chord-", seq_along(x), "-", sep = "") %>%
    tempfile(fileext = ".wav")
  for (i in seq_along(x)) save_wav_sox(x[[i]], files[i], ...)
  cmd <- c("sox", shQuote(files), shQuote(file),
           "reverb", reverb) %>%
    paste(collapse = " ")
  system(cmd)
  file.remove(files)
}

#' @export
save_wav_sox.sparse_fr_spectrum <- function(x,
                                            file,
                                            chord_length = 1,
                                            rise_length = 0.01,
                                            fade_length = 0.01,
                                            timbre = "sine",
                                            partial_volume = 0.1,
                                            spectrum_gain = "auto",
                                            volume = 0.1,
                                            ...) {
  fade_length <- fade_length
  frequencies <- freq(x)
  if (timbre == "pluck" && any(frequencies > 4000))
    stop("cannot play plucked notes with frequencies greater than 4,000 Hz")
  amplitudes <- amp(x) * volume
  tone_cmd <- purrr::map2_chr(frequencies, amplitudes, function(fr, amp) {
    '-v {amp} "|sox -n -p synth {chord_length} {timbre} {fr}"' %>%
      glue::glue()
  }) %>% paste(collapse = " ")
  norm_cmd <- if (spectrum_gain == "auto") "--norm=-3" else ""
  gain_cmd <- if (spectrum_gain == "auto") "" else paste0("gain ", spectrum_gain)
  "sox {norm_cmd} -m {tone_cmd} {file} {gain_cmd} fade {rise_length} {chord_length} {fade_length}" %>%
    glue::glue() %>%
    system()
}

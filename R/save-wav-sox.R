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
save_wav_sox <- function(x, file, chord_length = 1) {
  UseMethod("save_wav_sox")
}

#' @export
save_wav_sox.default <- function(x, ...) {
  save_wav_sox(pi_chord(x), ...)
}

#' @export
save_wav_sox.vec <- function(x, file, ...) {
  files <- paste("chord-", seq_along(x), "-", sep = "") %>%
    tempfile(fileext = ".wav")
  for (i in seq_along(x)) save_wav_sox(x[[i]], files[i], ...)
  cmd <- c("sox", shQuote(files), shQuote(file)) %>% paste(collapse = " ")
  system(cmd)
  file.remove(files)
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
save_wav_sox.fr_chord <- function(x, file, chord_length = 1) {
  checkmate::qassert(file, "S1")
  checkmate::qassert(chord_length, "N1(0,)")
  len <- sprintf("%.10f", as.numeric(chord_length))
  freq <- sprintf("%.10f", as.numeric(x))
  tones <- purrr::map_chr(freq,
                          ~ sprintf('"|sox -n -p synth %s pluck %s"', len, .)) %>%
    paste(collapse = " ")
  cmd <- sprintf('sox --norm=-3 -m %s %s', tones, file)
  system(cmd)
}

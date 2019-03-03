#' Play sound (pluck)
#'
#' Plays a sound with a plucked timbre.
#'
#' The sound is synthesised using \code{\link{save_wav_pluck}}
#' and saved to a temporary file, which is then played from the R session.
#'
#' @note
#' The command-line sound-processing program sox
#' (\url{http://sox.sourceforge.net/})
#' must be installed and available on the command line,
#' making available the commands \code{sox} and \code{play}.
#'
#' @param x Object to play (see \code{\link{save_wav_pluck}} for valid options).
#' @param ... Further parameters to pass to \code{\link{save_wav_pluck}}.
#'
#' @export
play_pluck <- function(x, ...) {
  UseMethod("play_pluck")
}

#' @export
play_pluck.default <- function(x, ...) {
  file <- tempfile(fileext = ".wav")
  save_wav_pluck(x, file = file, ...)
  system(paste0("play ", shQuote(file)))
  file.remove(file)
}

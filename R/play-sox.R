#' Play sound (sox)
#'
#' Plays a sound using the command-line tool sox.
#'
#' The sound is synthesised using \code{\link{save_wav_sox}}
#' and saved to a temporary file, which is then played from the R session.
#' This method is generally slower than \code{\link{play_wav}}
#' but it provides more features.
#'
#' @note
#' The command-line sound-processing program sox
#' (\url{http://sox.sourceforge.net/})
#' must be installed and available on the command line,
#' making available the commands \code{sox} and \code{play}.
#'
#' @param x Object to play (see \code{\link{save_wav_sox}} for valid options).
#' @param ... Further parameters to pass to \code{\link{save_wav_sox}}.
#'
#' @seealso \code{\link{play_wav}}
#'
#' @export
play_sox <- function(x, ...) {
  UseMethod("play_sox")
}

#' @export
play_sox.default <- function(x, ...) {
  file <- tempfile(fileext = ".wav")
  save_wav_sox(x, file = file, ...)
  system(paste0("play ", shQuote(file)))
  file.remove(file)
}

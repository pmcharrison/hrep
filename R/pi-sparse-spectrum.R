#' @export
.pi_sparse_spectrum <- function(pitch, amplitude) {
  checkmate::qassert(pitch, "N")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(pitch) == length(amplitude))

  res <- sparse_spectrum(x = pitch,
                         y = amplitude,
                         x_unit = "midi",
                         y_unit = "amplitude",
                         label = "pitch spectrum",
                         x_lab = "Pitch (MIDI)",
                         y_lab = "Amplitude (arbitrary units)")
  class(res) <- c("pi_sparse_spectrum", class(res))
  res
}

#' @export
pi_sparse_spectrum <- function(x, ...) {
  UseMethod("pi_sparse_spectrum")
}

#' @export
pi_sparse_spectrum.fr_sparse_spectrum <- function(x, ...) {
  .pi_sparse_spectrum(
    pitch = freq_to_midi(freq(x)),
    amplitude = amp(x)
  )
}

#' @export
pi_sparse_spectrum.default <- function(x, ...) {
  pi_sparse_spectrum(pi_chord(x), ...)
}

#' @export
#' @param ... Further arguments passed to \code{\link{expand_harmonics}()}.
pi_sparse_spectrum.pi_chord <- function(x,
                                        amplitude = 1,
                                        ...) {
  checkmate::qassert(amplitude, "N")
  if (length(amplitude) == 1L) amplitude <- rep_to_match(amplitude, x)
  stopifnot(length(amplitude) == length(x))
  expand_harmonics(.pi_sparse_spectrum(pitch = as.numeric(x),
                                       amplitude = amplitude),
                   ...)
}

#' @export
pitch.pi_sparse_spectrum <- function(x) {
  x$x
}

#' @export
`pitch.pi_sparse_spectrum<-` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(pitch(x)))
  x$x <- value
  x
}

#' @export
amp.pi_sparse_spectrum <- function(x) {
  x$y
}

#' @export
`amp.pi_sparse_spectrum<-` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(amp(x)))
  x$y <- value
  x
}

#' @param x_digits (Integerish scalar) Number of significant digits
#' to which pitches are rounded.
#' @export
c.pi_sparse_spectrum <- function(..., x_digits = 6) {
  combine_sparse_spectra_amplitudes(...,
                                    class = "pi_sparse_spectrum",
                                    constructor = .pi_sparse_spectrum,
                                    x_digits = x_digits)
}

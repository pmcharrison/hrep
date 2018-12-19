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

#' Pitch sparse spectrum
#'
#' This function represents an input object as a 'pitch sparse spectrum'.
#' @details
#' A pitch sparse spectrum describes an input sonority as a finite set
#' of spectral components, each defined by a
#' pitch (expressed on the MIDI pitch scale)
#' and an amplitude (expressed in arbitrary units, but with the
#' fundamental frequencies of chord pitches typically taking the value 1).
#' @param x Input sonority.
#' @param ... Further arguments passed to \code{\link{expand_harmonics}()},
#' depending on the method invoked.
#' @return An object of class \code{pi_sparse_spectrum}.
#' @rdname pi_sparse_spectrum
#' @export
pi_sparse_spectrum <- function(x, ...) {
  UseMethod("pi_sparse_spectrum")
}

#' @rdname pi_sparse_spectrum
#' @export
pi_sparse_spectrum.fr_sparse_spectrum <- function(x) {
  .pi_sparse_spectrum(
    pitch = freq_to_midi(freq(x)),
    amplitude = amp(x)
  )
}

#' @rdname pi_sparse_spectrum
#' @export
pi_sparse_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
  .pi_sparse_spectrum(pitch = x[[1]],
                      amplitude = x[[2]])
}

#' @rdname pi_sparse_spectrum
#' @export
pi_sparse_spectrum.default <- function(x, ...) {
  pi_sparse_spectrum(pi_chord(x), ...)
}

#' @rdname pi_sparse_spectrum
#' @export
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

#' @export
.fr_sparse_spectrum <- function(frequency, amplitude) {
  checkmate::qassert(frequency, "N")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(frequency) == length(amplitude))

  res <- sparse_spectrum(x = frequency,
                         y = amplitude,
                         x_unit = "Hz",
                         y_unit = "amplitude",
                         label = "frequency spectrum",
                         x_lab = "Frequency (Hz)",
                         y_lab = "Amplitude (arbitrary units)")
  class(res) <- c("fr_sparse_spectrum", class(res))
  res
}

#' Frequency sparse spectrum
#'
#' This function represents an input object as a
#' 'frequency sparse spectrum'.
#' @details
#' A frequency sparse spectrum comprises a finite set of spectral components,
#' each defined by a frequency (in Hz)
#' and an amplitude (expressed in arbitrary units, but with the
#' fundamental frequencies of chord pitches typically taking the value 1).
#' @param x Input sonority.
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' and expanded into their implied harmonics.
#' * Two-element lists will be treated as finalised spectra,
#' with the first element being a numeric vector of frequencies,
#' and the second element being a numeric vector of amplitudes.
#' @param ... Further arguments passed to \code{\link{expand_harmonics}()},
#' depending on the method invoked.
#' @return An object of class \code{fr_sparse_spectrum}.
#' @rdname fr_sparse_spectrum
#' @md
#' @export
fr_sparse_spectrum <- function(x, ...) {
  UseMethod("fr_sparse_spectrum")
}

#' @rdname fr_sparse_spectrum
#' @export
fr_sparse_spectrum.pi_sparse_spectrum <- function(x, ...) {
  .fr_sparse_spectrum(
    frequency = midi_to_freq(pitch(x)),
    amplitude = amp(x)
  )
}

#' @rdname fr_sparse_spectrum
#' @export
fr_sparse_spectrum.pi_chord <- function(x, ...) {
  fr_sparse_spectrum(pi_sparse_spectrum(x, ...))
}

#' @rdname fr_sparse_spectrum
#' @export
fr_sparse_spectrum.default <- function(x, ...) {
  fr_sparse_spectrum(pi_chord(x), ...)
}

#' @rdname fr_sparse_spectrum
#' @export
fr_sparse_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
  .fr_sparse_spectrum(frequency = x[[1]],
                      amplitude = x[[2]])
}

#' @export
freq.fr_sparse_spectrum <- function(x) {
  x$x
}

#' @export
`freq.fr_sparse_spectrum<-` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(freq(x)))
  x$x <- value
  x
}

#' @export
amp.fr_sparse_spectrum <- function(x) {
  x$y
}

#' @export
`amp.fr_sparse_spectrum<-` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(amp(x)))
  x$y <- value
  x
}

#' @param x_digits (Integerish scalar) Number of significant digits
#' to which frequencies are rounded.
#' @export
c.fr_sparse_spectrum <- function(..., x_digits = 6) {
  combine_sparse_spectra_amplitudes(...,
                                    class = "fr_sparse_spectrum",
                                    constructor = .fr_sparse_spectrum,
                                    x_digits = x_digits)
}

#' @keywords internal
.sparse_fr_spectrum <- function(frequency, amplitude) {
  checkmate::qassert(frequency, "N")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(frequency) == length(amplitude))

  res <- sparse_spectrum(x = frequency,
                         y = amplitude,
                         x_unit = "Hz",
                         y_unit = "amplitude",
                         label = "frequency spectrum",
                         x_lab = "Frequency (Hz)",
                         y_lab = "Amplitude")
  class(res) <- c("sparse_fr_spectrum", "chord", class(res))
  res
}

#' Sparse frequency spectrum
#'
#' This function represents an input object as a
#' sparse frequency spectrum.
#' @details
#' A sparse frequency spectrum comprises a finite set of spectral components,
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
#' @return An object of class \code{sparse_fr_spectrum}.
#' @rdname sparse_fr_spectrum
#' @md
#' @export
sparse_fr_spectrum <- function(x, ...) {
  UseMethod("sparse_fr_spectrum")
}

#' @rdname sparse_fr_spectrum
#' @export
sparse_fr_spectrum.sparse_pi_spectrum <- function(x, ...) {
  .sparse_fr_spectrum(
    frequency = midi_to_freq(pitch(x)),
    amplitude = amp(x)
  )
}

#' @rdname sparse_fr_spectrum
#' @export
sparse_fr_spectrum.pi_chord <- function(x, ...) {
  sparse_fr_spectrum(sparse_pi_spectrum(x, ...))
}

#' @rdname sparse_fr_spectrum
#' @export
sparse_fr_spectrum.default <- function(x, ...) {
  sparse_fr_spectrum(pi_chord(x), ...)
}

#' @rdname sparse_fr_spectrum
#' @export
sparse_fr_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
  .sparse_fr_spectrum(frequency = x[[1]],
                      amplitude = x[[2]])
}

#' @export
freq.sparse_fr_spectrum <- function(x) {
  x$x
}

#' @export
`freq<-.sparse_fr_spectrum` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(freq(x)))
  x$x <- value
  x
}

#' @export
amp.sparse_fr_spectrum <- function(x) {
  x$y
}

#' @export
`amp<-.sparse_fr_spectrum` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(amp(x)))
  x$y <- value
  x
}

#' @details
#' Sparse frequency spectra can be combined into one spectrum using \code{c(...)}.
#' Amplitudes are summed assuming incoherent wave superposition
#' (see \code{\link{sum_amplitudes}}).
#' @rdname sparse_fr_spectrum
#' @param x_digits (Integerish scalar) Number of significant digits
#' to which frequencies are rounded when being combined.
#' @export
c.sparse_fr_spectrum <- function(..., x_digits = 6) {
  combine_sparse_spectra_amplitudes(...,
                                    class = "sparse_fr_spectrum",
                                    constructor = .sparse_fr_spectrum,
                                    x_digits = x_digits)
}

.sparse_pi_spectrum <- function(pitch, amplitude) {
  checkmate::qassert(pitch, "N")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(pitch) == length(amplitude))

  res <- sparse_spectrum(x = pitch,
                         y = amplitude,
                         x_unit = "midi",
                         y_unit = "amplitude",
                         label = "sparse pitch spectrum",
                         x_lab = "Pitch (MIDI)",
                         y_lab = "Amplitude")
  class(res) <- c("sparse_pi_spectrum", "chord", class(res))
  res
}

#' Is sparse pitch spectrum
#'
#' Checks whether an object belongs to the class \code{sparse_pi_spectrum}.
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is.sparse_pi_spectrum <- function(x) {
  is(x, "sparse_pi_spectrum")
}

#' Sparse pitch spectrum
#'
#' This function represents an input object as a sparse pitch spectrum.
#'
#' @details
#' A sparse pitch spectrum describes an input sonority as a finite set
#' of spectral components, each defined by a
#' pitch (expressed on the MIDI pitch scale)
#' and an amplitude (expressed in arbitrary units, but with the
#' fundamental frequencies of chord pitches typically taking the value 1).
#'
#' A sparse pitch spectrum is most easily created by coercion from
#' a different chord representation. If a numeric vector is provided as the input,
#' it is treated as a \code{pi_chord} representation, for example
#' \code{sparse_pi_spectrum(c(60, 64, 67))} will create a sparse pitch spectrum
#' by expanding the harmonics implied by a C major triad.
#'
#' A sparse pitch spectrum can also be created directly by providing a list
#' with two elements, the first being labelled "pitch", and the second labelled "amplitude",
#' each of which being numeric vectors of the same length.
#' In this case no harmonic expansion is performed.
#' The first element will be taken as a vector of pitches,
#' and the second element will be taken as a vector of corresponding amplitudes.
#' For example, one might write
#' \code{sparse_pi_spectrum(list(pitch = c(60, 72, 79), amplitude = c(3, 2, 1)))}.
#'
#' @param x Input sonority.
#'
#' @inheritDotParams expand_harmonics
#'
#' @return An object of class \code{sparse_pi_spectrum}.
#'
#' @rdname sparse_pi_spectrum
#' @export
sparse_pi_spectrum <- function(x, ...) {
  UseMethod("sparse_pi_spectrum")
}

#' @rdname sparse_pi_spectrum
#' @export
sparse_pi_spectrum.sparse_pi_spectrum <- function(x, ...) {
  x
}

#' @rdname sparse_pi_spectrum
#' @export
sparse_pi_spectrum.sparse_fr_spectrum <- function(x, ...) {
  .sparse_pi_spectrum(
    pitch = freq_to_midi(freq(x)),
    amplitude = amp(x)
  )
}

#' @rdname sparse_pi_spectrum
#' @export
sparse_pi_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
  stopifnot(is.null(names(x)) || identical(names(x), c("pitch", "amplitude")))
  .sparse_pi_spectrum(pitch = x[[1]],
                      amplitude = x[[2]])
}

#' @rdname sparse_pi_spectrum
#' @export
sparse_pi_spectrum.default <- function(x, ...) {
  sparse_pi_spectrum(pi_chord(x), ...)
}

#' @param amplitude (Numeric vector)
#' Vector of amplitudes to assign to each pitch.
#' If a scalar value is provided, this value is assigned to all pitches
#'
#' @rdname sparse_pi_spectrum
#'
#' @export
sparse_pi_spectrum.pi_chord <- function(x,
                                        amplitude = 1,
                                        ...) {
  checkmate::qassert(amplitude, "N")
  if (length(amplitude) == 1L) amplitude <- rep_to_match(amplitude, x)
  stopifnot(length(amplitude) == length(x))
  expand_harmonics(.sparse_pi_spectrum(pitch = as.numeric(x),
                                       amplitude = amplitude),
                   ...)
}

#' @export
pitch.sparse_pi_spectrum <- function(x) {
  x$x
}

#' @export
`pitch<-.sparse_pi_spectrum` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(pitch(x)))
  x$x <- value
  x
}

#' @export
amp.sparse_pi_spectrum <- function(x) {
  x$y
}

#' @export
`amp<-.sparse_pi_spectrum` <- function(x, value) {
  stopifnot(is.numeric(value),
            length(value) == length(amp(x)))
  x$y <- value
  x
}

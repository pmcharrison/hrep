.sparse_pc_spectrum <- function(pc, amplitude, labels = NULL) {
  checkmate::qassert(pc, "N[0,12)")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(pc) == length(amplitude))

  res <- sparse_spectrum(x = pc,
                         y = amplitude,
                         x_unit = "midi",
                         y_unit = "amplitude",
                         label = "sparse pitch-class spectrum",
                         x_lab = "Pitch class",
                         y_lab = "Amplitude",
                         labels = labels)
  class(res) <- c("sparse_pc_spectrum", "chord", class(res))
  res
}

#' Is sparse pitch-class spectrum
#'
#' Checks whether an object belongs to the class \code{sparse_pc_spectrum}.
#'
#' @param x Object to check.
#'
#' @return Logical scalar.
#'
#' @export
is.sparse_pc_spectrum <- function(x) {
  is(x, "sparse_pc_spectrum")
}

#' Sparse pitch-class spectrum
#'
#' This function represents an input object as a sparse pitch-class spectrum.
#'
#' @details
#' A sparse pitch-class spectrum describes an input sonority as a finite set
#' of spectral components, each defined by a
#' pitch class (expressed as numbers in the range [0, 12))
#' and an amplitude (expressed in arbitrary units, but with the
#' fundamental frequencies of chord pitches typically taking the value 1).
#'
#' A sparse pitch-class spectrum is most easily created by coercion from
#' a different chord representation. If a numeric vector is provided as the input,
#' it is treated as a \code{pi_chord} representation, for example
#' \code{sparse_pc_spectrum(c(60, 64, 67))} will create a sparse pitch-class spectrum
#' by expanding the harmonics implied by a C major triad.
#'
#' A sparse pitch-class spectrum can also be created directly by providing a list
#' with two elements, the first being labelled "pc", and the second labelled "amplitude",
#' each of which being numeric vectors of the same length.
#' In this case no harmonic expansion is performed.
#' The first element will be taken as a vector of pitches,
#' and the second element will be taken as a vector of corresponding amplitudes.
#' For example, one might write
#' \code{sparse_pc_spectrum(list(pitch = c(0, 4, 7), amplitude = c(3, 1, 2)))}.
#'
#' @param x Input sonority.
#'
#' @inheritParams collapse_summing_amplitudes
#' @inheritDotParams expand_harmonics
#'
#' @return An object of class \code{sparse_pc_spectrum}.
#'
#' @rdname sparse_pc_spectrum
#'
#' @export
sparse_pc_spectrum <- function(x, ...) {
  UseMethod("sparse_pc_spectrum")
}

sparse_pc_spectrum.sparse_pc_spectrum <- function(x, ...) {
  x
}

sparse_pc_spectrum.sparse_pi_spectrum <- function(x, digits = 6, coherent = FALSE, ...) {
  df <- data.frame(x = pitch(x),
                   y = amp(x))
  if (!is.null(x$labels)) df$labels <- x$labels
  df %>%
    list() %>%
    collapse_summing_amplitudes(digits = digits, modulo = 12, coherent = coherent) %>%
    {
      .sparse_pc_spectrum(pc = .[[1]],
                          amplitude = .[[2]],
                          labels = .$labels)
    }
}

#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.sparse_fr_spectrum <- function(x, coherent = FALSE, ...) {
  sparse_pc_spectrum(sparse_pi_spectrum(x, coherent = coherent), coherent = coherent, ...)
}

#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
  stopifnot(is.null(names(x)) || identical(names(x), c("pc", "amplitude")))
  .sparse_pc_spectrum(pc = x[[1]],
                      amplitude = x[[2]])
}

#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.default <- function(x, ...) {
  sparse_pc_spectrum(pi_chord(x), ...)
}

#' @param amplitude (Numeric vector)
#' Vector of amplitudes to assign to each pitch.
#' If a scalar value is provided, this value is assigned to all pitches
#'
#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.pi_chord <- function(x,
                                        amplitude = 1,
                                        coherent = FALSE,
                                        ...) {
  sparse_pc_spectrum(sparse_pi_spectrum(x, amplitude = amplitude, coherent = coherent, ...),
                     coherent = coherent)
}

#' @export
pc.sparse_pc_spectrum <- function(x) {
  x$x
}

#' @export
`pc<-.sparse_pc_spectrum` <- function(x, value) {
  .sparse_pc_spectrum(pc = value,
                      amplitude = amp(x))
}

#' @export
amp.sparse_pc_spectrum <- function(x) {
  x$y
}

#' @export
`amp<-.sparse_pc_spectrum` <- function(x, value) {
  .sparse_pc_spectrum(pc = pc(x),
                      amplitude = value)
}

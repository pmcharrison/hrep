.sparse_pc_spectrum <- function(pc, amplitude) {
  checkmate::qassert(pc, "N[0,12)")
  checkmate::qassert(amplitude, "N")
  stopifnot(length(pc) == length(amplitude))

  res <- sparse_spectrum(x = pc,
                         y = amplitude,
                         x_unit = "midi",
                         y_unit = "amplitude",
                         label = "sparse pitch-class spectrum",
                         x_lab = "Pitch class",
                         y_lab = "Amplitude")
  class(res) <- c("sparse_pc_spectrum", "chord", class(res))
  res
}

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
#' @param x Input sonority.
#'
#' @param ... Further arguments passed to \code{\link{expand_harmonics}()},
#' depending on the method invoked.
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

sparse_pc_spectrum.sparse_pi_spectrum <- function(x, digits = 6) {
  data.frame(x = pitch(x),
             y = amp(x)) %>%
    list() %>%
    collapse_summing_amplitudes(digits = digits, modulo = 12) %>%
    {
      .sparse_pc_spectrum(pc = .[[1]],
                          amplitude = .[[2]])
    }
}

#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.sparse_fr_spectrum <- function(x, ...) {
  sparse_pc_spectrum(sparse_pi_spectrum(x))
}

#' @rdname sparse_pc_spectrum
#' @export
sparse_pc_spectrum.list <- function(x, ...) {
  stopifnot(length(x) == 2L,
            is.numeric(x[[1]]),
            is.numeric(x[[2]]),
            length(x[[1]]) == length(x[[2]]))
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
                                        ...) {
  sparse_pc_spectrum(sparse_pi_spectrum(x, amplitude = amplitude, ...))
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

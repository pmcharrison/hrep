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

#' @export
fr_sparse_spectrum <- function(x, ...) {
  UseMethod("fr_sparse_spectrum")
}

#' @export
fr_sparse_spectrum.pi_sparse_spectrum <- function(x) {
  .fr_sparse_spectrum(
    frequency = midi_to_freq(pitch(x)),
    amplitude = amp(x)
  )
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

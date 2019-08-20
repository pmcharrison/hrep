#' wave constructor
#'
#' Constructor function for objects of class "wave".
#'
#' @param x Numeric vector of displacement values.
#' @param sample_rate (Numeric scalar) Sample rate.
#'
#' @export
.wave <- function(x, sample_rate) {
  checkmate::qassert(x, "N")
  checkmate::qassert(sample_rate, "X1")
  x <- as.numeric(x)
  class(x) <- c("wave", class(x))
  sample_rate(x) <- sample_rate
  x
}

#' @export
print.wave <- function(x, ...) {
  cat(
    "Wave object\n",
    "  Sample rate = ", sample_rate(x), "\n",
    "  Number of samples = ", length(x), "\n",
    "  Duration = ", length(x) / sample_rate(x), " seconds\n",
    sep = ""
  )
}

#' Wave
#'
#' This function represents an object as class "wave".
#' Under the hood, "wave" objects are numeric vectors
#' describing displacement as a function of time.
#' The sample rate can be accessed using the \code{\link{sample_rate}} accessor.
#'
#' @param x Input object.
#' @param ... Arguments to be passed to \code{\link{sparse_fr_spectrum}}, as appropriate.
#'
#' @rdname wave
#' @export
wave <- function(x, ...) {
  UseMethod("wave")
}

#' @param length_sec (Numeric scalar) Length of the output wave, in seconds.
#' @param sample_rate (Integerish scalar) The desired sample rate.
#'
#' @rdname wave
#' @export
wave.default <- function(x, length_sec = 1, sample_rate = 44100, ...) {
  x <- sparse_fr_spectrum(x, ...)
  wave(x, length_sec = length_sec, sample_rate = sample_rate)
}

#' @export
wave.wave <- function(x, ...) x

#' @rdname wave
#' @export
wave.sparse_fr_spectrum <- function(x, length_sec = 1, sample_rate = 44100, ...) {
  checkmate::qtest(length_sec, "N1[0)")
  checkmate::qtest(sample_rate, "X1[1)")
  frequency <- freq(x)
  amplitude <- amp(x)
  num_samples <- sample_rate * length_sec
  time <- seq(from = 0,
              to = length_sec,
              length.out = num_samples + 1)[- (num_samples + 1)]
  mapply(
    function(frequency, amplitude) {
      amplitude * sin(2 * pi * frequency * time)
    }, frequency, amplitude
  ) %>%
    rowMeans() %>%
    .wave(sample_rate)
}

#' @export
plot.wave <- function(x, ggplot = FALSE, xlab = "Time (seconds)", ylab = "Displacement",
                      ylim = NULL, ...) {
  time <- seq(from = 0, by = 1 / sample_rate(x), length.out = length(x))
  if (ggplot) {
    tibble::tibble(time = time, displacement = as.numeric(x)) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "time", y = "displacement")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(xlab, limits = c(0, time[length(time)])) +
      ggplot2::scale_y_continuous(ylab, limits = ylim)
  } else {
    plot(time, x, xlab = xlab, ylab = ylab, type = "l", ylim = ylim)
  }
}

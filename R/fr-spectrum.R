#' @export
plot_spectrum <- function( # nocov start
  frequency,
  amplitude,
  range_Hz = NULL,
  theme = ggplot2::theme_bw()
) {
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  order <- order(frequency)
  df <- data.frame(freq = frequency[order], amp = amplitude[order]) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ])
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme
  print(p)
} # nocov end

#' @export
plot_sparse_spectrum <- function( # nocov start
  frequency, amplitude,
  resolution_Hz = 1,
  range_Hz = NULL,
  theme = ggplot2::theme_bw()
) {
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  df <- data.frame(freq = round(frequency),
                   amp = amplitude) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ]) %>%
    rbind(
      data.frame(freq = seq(from = range_Hz[1],
                            to = range_Hz[2],
                            by = resolution_Hz),
                 amp = 0)
    ) %>%
    (function(df) {
      df[order(df$freq), ]
    })
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme
  print(p)
} # nocov end

#' @export
play_sparse_spectrum <- function( # nocov start
  frequency, amplitude,
  sample_rate = 44e3,
  seconds = 1,
  bit = 16
) {
  spectrum <- sparse_spectrum_to_waveform(
    frequency = frequency, amplitude = amplitude,
    seconds = seconds, sample_rate = sample_rate, bit = bit
  )
  tuneR::play(tuneR::Wave(spectrum$y, samp.rate = sample_rate, bit = bit),
              "play")
} # nocov end

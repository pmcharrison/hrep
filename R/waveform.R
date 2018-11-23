#' @export
plot_waveform <- function( # nocov start
  x,
  sample_rate = 44000,
  range_sec = c(0, 0.2),
  theme = ggplot2::theme_bw()
) {
  df <- data.frame(
    t = seq(from = 0, by = 1 / sample_rate, length.out = length(x)),
    x = x
  ) %>%
    (function(df) df[df$t >= range_sec[1] & df$t <= range_sec[2], ])
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "t", y = "x")) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_x_continuous("Time (sec)") +
    ggplot2::scale_y_continuous("Instantaneous amplitude") +
    theme
  print(p)
} # nocov end

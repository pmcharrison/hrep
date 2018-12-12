# #' @export
# plot_spectrum <- function( # nocov start
#   frequency,
#   amplitude,
#   range_Hz = NULL,
#   theme = ggplot2::theme_bw()
# ) {
#   if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
#   order <- order(frequency)
#   df <- data.frame(freq = frequency[order], amp = amplitude[order]) %>%
#     (function(df) df[df$freq >= range_Hz[1] &
#                        df$freq <= range_Hz[2], ])
#   p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
#     ggplot2::geom_line() +
#     ggplot2::scale_x_continuous("Frequency (Hz)") +
#     ggplot2::scale_y_continuous("Amplitude") +
#     theme
#   print(p)
# } # nocov end
#
#


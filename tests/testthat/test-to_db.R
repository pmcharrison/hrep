# context("test-to_db")
#
# library(magrittr)
#
# test_that("examples", {
#   a <- 1:5
#   .pi_sparse_spectrum(1:5, a, dB = FALSE) %>%
#     to_dB() %T>%
#     expect_is("pi_sparse_spectrum") %>%
#     {.$y} %>%
#     expect_equal(amplitude_to_dB(a, unit_amplitude_in_dB = 60))
#
#   a <- 1:5
#   .pi_sparse_spectrum(1:5, a, dB = FALSE) %>%
#     to_dB(unit_amplitude_in_dB = 50) %T>%
#     expect_is("pi_sparse_spectrum") %>%
#     {.$y} %>%
#     expect_equal(amplitude_to_dB(a, unit_amplitude_in_dB = 50))
# })

# context("test-infer_type")
#
# library(purrr)
#
# test_that("examples", {
#   list(c(0, 4, 7), c(0, 3, 7)) %>%
#     map(pc_set) %>%
#     infer_type %>%
#     expect_equal("pc_set")
#
#   list(c(0, 4, 7), c(0, 3, 7)) %>%
#     map(pi_chord) %>%
#     infer_type %>%
#     expect_equal("pc_set")
# })

context("pc_set_to_pc_spectrum")

test_that("Compare results to saved results from prior code versions", {
  expect_equal(
    c(0, 4, 7) %>% pc_set %>% pc_smooth_spectrum() %>% as.numeric,
    readRDS("pc_spectrum_example_outputs/0-4-7.rds")
  )
  expect_equal(
    c(2, 5, 9) %>% pc_set %>% pc_smooth_spectrum() %>% as.numeric,
    readRDS("pc_spectrum_example_outputs/2-5-9.rds")
  )
  expect_equal(
    c(1, 3, 2) %>% pc_set %>% pc_smooth_spectrum() %>% as.numeric,
    readRDS("pc_spectrum_example_outputs/1-3-2.rds")
  )
})

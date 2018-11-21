context("convert_pc_set_to_pc_spectrum")

test_that("Compare results to saved results from prior code versions", {
  expect_equal(
    convert_pc_set_to_pc_spectrum(c(0, 4, 7))@values,
    readRDS("pc_spectrum_example_outputs/0-4-7.rds")
  )
  expect_equal(
    convert_pc_set_to_pc_spectrum(c(2, 5, 9))@values,
    readRDS("pc_spectrum_example_outputs/2-5-9.rds")
  )
  expect_equal(
    convert_pc_set_to_pc_spectrum(c(1, 3, 2))@values,
    readRDS("pc_spectrum_example_outputs/1-3-2.rds")
  )
})

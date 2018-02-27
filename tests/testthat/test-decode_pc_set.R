library(testthat)

context("decode_pc_set")

test_that("examples", {
  expect_equal(
    decode_pc_set(20),
    decode_pc_sets(20)[[1]]
  )
})

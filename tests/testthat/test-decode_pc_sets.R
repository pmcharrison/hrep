library(testthat)
library(hutil)

context("decode_pc_sets")

test_that("bad inputs", {
  expect_error(
    decode_pc_sets(0)
  )
  expect_error(
    decode_pc_sets(-1)
  )
  expect_error(
    decode_pc_sets(4100)
  )
  expect_error(
    decode_pc_sets(c(2, 5, 4100))
  )
  expect_error(
    decode_pc_sets(c(2, 0, 5))
  )
  expect_error(
    decode_pc_sets(c(-1, 2, 5))
  )
  expect_error(
    decode_pc_sets(NA)
  )
})

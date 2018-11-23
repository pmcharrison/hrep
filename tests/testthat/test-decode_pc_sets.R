library(testthat)
library(hrep)

context("decode_pc_sets")

test_that("bad inputs", {
  expect_error(
    decode(0, type = "pc_set")
  )
  expect_error(
    decode(-1, type = "pc_set")
  )
  expect_error(
    decode(4100, type = "pc_set")
  )
  expect_error(
    decode(c(2, 5, 4100), type = "pc_set")
  )
  expect_error(
    decode(c(2, 0, 5), type = "pc_set")
  )
  expect_error(
    decode(c(-1, 2, 5), type = "pc_set")
  )
  expect_error(
    decode(NA, type = "pc_set")
  )
})

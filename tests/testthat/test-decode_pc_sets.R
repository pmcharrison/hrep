library(testthat)
library(hrep)

context("decode_pc_sets")

test_that("bad inputs", {
  expect_error(
    0 %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    -1 %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    4100 %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    c(2, 5, 4100) %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    c(2, 0, 5) %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    c(-1, 2, 5) %>% coded_vec("pc_set") %>% decode()
  )
  expect_error(
    NA %>% coded_vec("pc_set") %>% decode()
  )
})

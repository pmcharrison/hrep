library(testthat)

context("normalise_pc_set_transition")

library(magrittr)

test_that("perfect cadence", {
  trans <- c(
    c(0, 4, 7) %>% pc_set %>% transpose(10) %>% encode,
    c(0, 4, 7) %>% pc_set %>% transpose(3) %>% encode
  )
  norm <- c(
    c(0, 4, 7) %>% pc_set %>% transpose(0) %>% encode,
    c(0, 4, 7) %>% pc_set %>% transpose(-7) %>% encode
  )
  expect_equal(
    normalise_pc_set_transition(trans[1], trans[2]),
    norm
  )
})

test_that("example with NA", {
  trans <- c(0, 3, 7) %>% pc_set %>% transpose(3) %>% encode
  norm <- c(0, 3, 7) %>% pc_set %>% transpose(0) %>% encode
  expect_equal(
    normalise_pc_set_transition(NA, trans),
    c(NA, norm)
  )
})

library(testthat)

context("normalise_pc_set_transition")

library(HarmonyStats)
library(HarmonyUtils)
library(magrittr)

test_that("perfect cadence", {
  trans <- c(
    c(0, 4, 7) %>% new_pc_set %>% transpose(10) %>% encode_pc_set,
    c(0, 4, 7) %>% new_pc_set %>% transpose(3) %>% encode_pc_set
  )
  norm <- c(
    c(0, 4, 7) %>% new_pc_set %>% transpose(0) %>% encode_pc_set,
    c(0, 4, 7) %>% new_pc_set %>% transpose(-7) %>% encode_pc_set
  )
  expect_equal(
    normalise_pc_set_transition(trans[1], trans[2]),
    norm
  )
})

test_that("example with NA", {
  trans <- c(0, 3, 7) %>% new_pc_set %>% add(3) %>% encode_pc_set
  norm <- c(0, 3, 7) %>% new_pc_set %>% add(0) %>% encode_pc_set
  expect_equal(
    normalise_pc_set_transition(NA, trans),
    c(NA, norm)
  )
})


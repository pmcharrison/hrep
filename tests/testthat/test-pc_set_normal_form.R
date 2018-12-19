library(testthat)
library(magrittr)

context("pc_set_norm_form")

test_that("examples", {
  expect_equal(
    pc_set_norm_form(c(0, 4, 7)) %>% as.integer,
    c(0, 4, 7)
  )
  expect_equal(
    pc_set_norm_form(c(3, 6, 7, 10)) %>% as.integer,
    c(0, 3, 4, 7)
  )
  expect_equal(
    pc_set_norm_form(c(0, 3, 6, 7, 8, 10)) %>% as.integer,
    c(0, 1, 2, 4, 6, 9)
  )
  expect_equal(
    pc_set_norm_form(pc_set(0)) %>% as.integer,
    0
  )
  expect_equal(
    pc_set_norm_form(pc_set(5)) %>% as.integer,
    0
  )
})

test_that("transposition", {
  for (i in 0:11) {
    expect_equal(
      c(0, 4, 7) %>% pc_set %>% tp(i) %>%
        pc_set_norm_form %>%
         transposition,
      - i
    )
  }
})

test_that("transpositions of a pitch-class set all get the same normal form", {
  n <- 30
  for (i in seq_len(n)) {
    pc_set <- sample(4e3, 1) %>% coded_vec("pc_set") %>% decode %>% extract2(1)
    expect_true(
      0:11 %>%
        lapply(function(x) tp(pc_set, x)) %>%
        lapply(pc_set_norm_form) %>%
        lapply(as.integer) %>%
        unique %>%
        (function(x) length(x) == 1L)
    )
  }
})

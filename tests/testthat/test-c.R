context("test-c")

library(magrittr)

test_that("pc_set", {
  expect_equal(
    c(pc_set(c(0, 3, 7)), pc_set(c(0, 4, 7))),
    pc_set(c(0, 3, 4, 7))
  )
  expect_equal(
    c(pc_set(c(0, 1, 5)), pc_set(c(3, 7, 8))),
    pc_set(c(0, 1, 3, 5, 7, 8))
  )
})

test_that("pc_set_norm_form", {
  expect_equal(
    list(pc_set(c(2, 6, 9)), pc_set(c(3, 6, 10))) %>%
      lapply(pc_set_norm_form) %>%
      do.call(c, .),
    pc_set(c(0, 3, 4, 7))
  )
})

test_that("pc_set_norm_order", {
  expect_equal(
    list(pc_set(c(0, 1, 2)), pc_set(c(0, 10, 11))) %>%
      lapply(pc_set_norm_order) %>%
      do.call(c, .),
    pc_set(c(0, 1, 2, 10, 11))
  )
})

test_that("pi_chord", {
  expect_equal(
    c(pi_chord(c(60, 63, 67)), pi_chord(c(60, 64, 67))),
    pi_chord(c(60, 63, 64, 67))
  )
  expect_equal(
    c(pi_chord(c(60, 61, 65)), pi_chord(c(63, 67, 68))),
    pi_chord(c(60, 61, 63, 65, 67, 68))
  )
})

test_that("smooth_spectrum", {
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equal(
    c(
      smooth_spectrum(x, "x", "y", 0, 10, TRUE, TRUE, "test"),
      smooth_spectrum(y, "x", "y", 0, 10, TRUE, TRUE, "test")
    ),
    c(x, y)
  )
})

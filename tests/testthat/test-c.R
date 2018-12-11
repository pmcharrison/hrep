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

test_that("fr_sparse_spectrum", {
  x <- .fr_sparse_spectrum(c(100, 200, 300), c(1, 1, 1))
  y <- .fr_sparse_spectrum(c(200, 300, 400), c(1, 1, 1))
  z <- c(x, y)
  expect_is(z, "fr_sparse_spectrum")
  expect_equal(freq(z), 100 * 1:4)
  expect_equal(amp(z),
               c(1, sum_amplitudes(c(1, 1), c(1, 1)), 1))
})

test_that("pi_sparse_spectrum", {
  x <- .pi_sparse_spectrum(c(100, 200, 300), c(1, 1, 1))
  y <- .pi_sparse_spectrum(c(200, 300, 400), c(1, 1, 1))
  z <- c(x, y)
  expect_is(z, "pi_sparse_spectrum")
  expect_equal(pitch(z), 100 * 1:4)
  expect_equal(amp(z),
               c(1, sum_amplitudes(c(1, 1), c(1, 1)), 1))
})

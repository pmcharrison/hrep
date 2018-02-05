context("get_chord_alphabet_size")

test_that("answer", {
  expect_equal(get_chord_alphabet_size(),
               12 * 2 ^ 11)
})

context("pc_chord_alphabet_size")

test_that("answer", {
  expect_equal(pc_chord_alphabet_size(),
               12 * 2 ^ 11)
})

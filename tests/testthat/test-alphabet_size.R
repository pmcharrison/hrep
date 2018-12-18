context("test-alphabet_size")

test_that("examples", {
  expect_equal(alphabet_size("pc_chord"), 24576)
  expect_equal(alphabet_size("pc_chord_type"), 24576 / 12)
  expect_equal(alphabet_size("pc_set"), 4095)
})

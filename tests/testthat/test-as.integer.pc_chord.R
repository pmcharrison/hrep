context("as.integer.chord")

test_that("example", {
  chord <- pc_chord(1, c(3, 6, 7))
  res <- as.integer(chord)
  expect_is(res, "integer")
  expect_equal(res, c(1, 3, 6, 7))
  expect_equal(as.pc_chord(res), chord)
})

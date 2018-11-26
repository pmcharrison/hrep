context("as.integer.chord")

test_that("example", {
  chord <- pc_chord(c(1, 3, 6, 7))
  res <- as.integer(chord)
  expect_is(res, "integer")
  expect_equal(res, c(1, 3, 6, 7))
  expect_equal(pc_chord(res), chord)
})

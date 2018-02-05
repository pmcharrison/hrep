context("as.integer.Chord")

test_that("example", {
  chord <- make_chord(1, c(3, 6, 7))
  res <- as.integer(chord)
  expect_is(res, "integer")
  expect_equal(res, c(49, 63, 66, 67))
  expect_equal(as.Chord(res), chord)
})

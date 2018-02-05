context("as.Chord")

test_that("example", {
  expect_equal(
    as.Chord(c(48L, 64L, 67L)),
    make_chord(0, c(4, 7))
  )
  expect_equal(
    as.Chord(c(48L, 67L, 64L)),
    make_chord(0, c(4, 7))
  )
  expect_equal(
    as.Chord(c(0L, 5L, 7L)),
    make_chord(0, c(5, 7))
  )
})

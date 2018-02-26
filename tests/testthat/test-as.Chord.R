context("as.chord")

test_that("example", {
  expect_equal(
    as.chord(c(48L, 64L, 67L)),
    new_chord(0, c(4, 7))
  )
  expect_equal(
    as.chord(c(48L, 67L, 64L)),
    new_chord(0, c(4, 7))
  )
  expect_equal(
    as.chord(c(0L, 5L, 7L)),
    new_chord(0, c(5, 7))
  )
})

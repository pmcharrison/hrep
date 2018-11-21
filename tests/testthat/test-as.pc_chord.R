context("as.pc_chord")

test_that("example", {
  expect_equal(
    as.pc_chord(c(48L, 64L, 67L)),
    pc_chord(0, c(4, 7))
  )
  expect_equal(
    as.pc_chord(c(48L, 67L, 64L)),
    pc_chord(0, c(4, 7))
  )
  expect_equal(
    as.pc_chord(c(0L, 5L, 7L)),
    pc_chord(0, c(5, 7))
  )
})

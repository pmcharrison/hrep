context("normalise_bass")

test_that("examples", {
  expect_equal(
    normalise_bass(pc_chord(4, c(7, 11))),
    pc_chord(0, c(3, 7))
  )
  expect_equal(
    normalise_bass(pc_chord(10, c(4, 7))),
    pc_chord(0, c(6, 9))
  )
})

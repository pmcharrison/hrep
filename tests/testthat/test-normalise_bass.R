context("normalise_bass")

test_that("examples", {
  expect_equal(
    normalise_bass(pc_chord(c(4, 7, 11))),
    pc_chord(c(0, 3, 7))
  )
  expect_equal(
    normalise_bass(pc_chord(c(10, 4, 7))),
    pc_chord(c(0, 6, 9))
  )
})

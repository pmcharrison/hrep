context("normalise_bass")

test_that("pc_chord", {
  expect_equal(
    normalise_bass(pc_chord(c(4, 7, 11))),
    pc_chord(c(0, 3, 7))
  )
  expect_equal(
    normalise_bass(pc_chord(c(10, 4, 7))),
    pc_chord(c(0, 6, 9))
  )
})

test_that("pi_chord", {
  expect_equal(
    normalise_bass(pi_chord(c(4, 7, 11))),
    pi_chord(c(0, 3, 7))
  )
  expect_equal(
    normalise_bass(pi_chord(c(60, 64, 67))),
    pi_chord(c(0, 4, 7))
  )
})

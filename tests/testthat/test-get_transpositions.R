context("get_transpositions")

test_that("invariance", {
  c1 <- pc_chord(3, c(5, 9))
  c2 <- transpose(c1, 3)
  expect_equal(
    get_transpositions(c1),
    get_transpositions(c2)
  )
})

test_that("example", {
  c1 <- pc_chord(2, c(3, 5))
  expect_equal(
    get_transpositions(c1),
    list(
      pc_chord(0, c(1, 3)),
      pc_chord(1, c(2, 4)),
      pc_chord(2, c(3, 5)),
      pc_chord(3, c(4, 6)),
      pc_chord(4, c(5, 7)),
      pc_chord(5, c(6, 8)),
      pc_chord(6, c(7, 9)),
      pc_chord(7, c(8, 10)),
      pc_chord(8, c(9, 11)),
      pc_chord(9, c(10, 0)),
      pc_chord(10, c(11, 1)),
      pc_chord(11, c(0, 2))
    )
  )
})

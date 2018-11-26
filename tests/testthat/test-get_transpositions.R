context("get_transpositions")

test_that("invariance", {
  c1 <- pc_chord(c(3, 5, 9))
  c2 <- transpose(c1, 3)
  expect_equal(
    get_transpositions(c1),
    get_transpositions(c2)
  )
})

test_that("example", {
  c1 <- pc_chord(c(2, 3, 5))
  expect_equal(
    get_transpositions(c1),
    list(
      pc_chord(c(0, 1, 3)),
      pc_chord(c(1, 2, 4)),
      pc_chord(c(2, 3, 5)),
      pc_chord(c(3, 4, 6)),
      pc_chord(c(4, 5, 7)),
      pc_chord(c(5, 6, 8)),
      pc_chord(c(6, 7, 9)),
      pc_chord(c(7, 8, 10)),
      pc_chord(c(8, 9, 11)),
      pc_chord(c(9, 10, 0)),
      pc_chord(c(10, 11, 1)),
      pc_chord(c(11, 0, 2))
    )
  )
})

context("pc_chord")

test_that("random examples", {
  chord <- pc_chord(2, c(4, 7))
  expect_equal(
    get_bass_pc(chord), 2
  )
  expect_equal(
    get_non_bass_pc(chord), pc_set(c(4, 7))
  )
  expect_equal(
    as.numeric(as.pc_set(chord)),
    c(2, 4, 7)
  )
  expect_equal(
    pc_chord(10, c(3, 4)) %>% as.pc_set %>% as.numeric,
    c(3, 4, 10)
  )
})

test_that("equivalences", {
  expect_equal(
    pc_chord(3, c(5, 7, 9)),
    pc_chord(3, c(9, 7, 5))
  )
  expect_equal(
    pc_chord(3, c(3, 5, 7, 9)),
    pc_chord(3, c(9, 7, 5))
  )
})

test_that("input checking", {
  expect_error(pc_chord(c(1, 2), 1:3))
  expect_error(pc_chord(c(12), 1:3))
  expect_error(pc_chord(1, 1:13))
})

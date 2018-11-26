context("pc_chord")

test_that("pi_chord", {
  expect_equal(
    pc_chord(pi_chord(c(48L, 64L, 67L))),
    pc_chord(c(0, 4, 7))
  )
  expect_equal(
    pc_chord(pi_chord(c(48L, 67L, 64L))),
    pc_chord(c(0, 4, 7))
  )
  expect_equal(
    pc_chord(pi_chord(c(0L, 5L, 7L))),
    pc_chord(c(0, 5, 7))
  )
})

test_that("pc_set", {
  expect_equal(
    pc_chord(pc_set(c(0, 4, 7))),
    pc_chord(c(0, 4, 7))
  )
  expect_equal(
    pc_chord(pc_set(c(5, 8, 9))),
    pc_chord(c(5, 8, 9))
  )
})

test_that("random examples", {
  chord <- pc_chord(c(2, 4, 7))
  expect_equal(
    get_bass_pc(chord), 2
  )
  expect_equal(
    get_non_bass_pc(chord), pc_set(c(4, 7))
  )
  expect_equal(
    as.numeric(pc_set(chord)),
    c(2, 4, 7)
  )
  expect_equal(
    pc_chord(c(10, 3, 4)) %>% pc_set %>% as.numeric,
    c(3, 4, 10)
  )
})

test_that("equivalences", {
  expect_equal(
    pc_chord(c(3, 5, 7, 9)),
    pc_chord(c(3, 9, 7, 5))
  )
})

context("transpose")

library(magrittr)

test_that("example 1", {
  c1 <- pc_chord(8, c(6, 9, 10))
  c2 <- transpose(c1, 4)
  expect_equal(
    get_bass_pc(c2), 0
  )
  expect_equal(
    get_non_bass_pc(c2), c(1, 2, 10)
  )
})

test_that("reversible", {
  n <- 10
  for (i in seq_len(10)) {
    c1 <- pc_chord(sample(11, 1), sample(11, 4))
    int <- sample(-11:11, 1)
    c2 <- transpose(c1, int)
    c3 <- transpose(c2, -int)
    expect_equal(c1, c3)
  }
})

test_that("pc_set", {
  p1 <- pc_set(c(0, 4, 7))
  expect_is(transpose(p1, 2), "pc_set")
  expect_equal(
    transpose(p1, 2) %>% as.integer,
    c(2, 6, 9)
  )
  expect_equal(
    c(0, 4, 7) %>% pc_set %>% transpose(-2) %>% as.integer,
    c(2, 5, 10)
  )
})

test_that("+/- alias for pc_set", {
  for (n in 1:30) {
    pc_set <- decode_pc_sets(sample(4e3, 1))[[1]]
    int <- sample(12L, 1L) - 1L
    expect_equal(
      transpose(pc_set, int),
      pc_set + int
    )
    expect_equal(
      pc_set + int,
      int + pc_set
    )
    expect_equal(
      transpose(pc_set, - int),
      pc_set - int
    )
  }
})

test_that("+/- alias for chord", {
  for (n in 1:30) {
    chord <- decode_chord(sample(2e4, 1))
    int <- sample(12L, 1L) - 1L
    expect_equal(
      transpose(chord, int),
      chord + int
    )
    expect_equal(
      chord + int,
      int + chord
    )
    expect_equal(
      transpose(chord, - int),
      chord - int
    )
  }
})

context("transpose")

library(magrittr)

test_that("example 1", {
  c1 <- make_chord(8, c(6, 9, 10))
  c2 <- HarmonyUtils::transpose(c1, 4)
  expect_equal(
    HarmonyUtils::get_bass_pc(c2), 0
  )
  expect_equal(
    HarmonyUtils::get_non_bass_pc_set(c2), c(1, 2, 10)
  )
})

test_that("reversible", {
  n <- 10
  for (i in seq_len(10)) {
    c1 <- make_chord(sample(11, 1), sample(11, 4))
    int <- sample(-11:11, 1)
    c2 <- transpose(c1, int)
    c3 <- transpose(c2, -int)
    expect_equal(c1, c3)
  }
})

test_that("pc_set", {
  p1 <- make_pc_set(c(0, 4, 7))
  expect_is(transpose(p1, 2), "pc_set")
  expect_equal(
    transpose(p1, 2) %>% as.integer,
    c(2, 6, 9)
  )
  expect_equal(
    c(0, 4, 7) %>% make_pc_set %>% transpose(-2) %>% as.integer,
    c(2, 5, 10)
  )
})

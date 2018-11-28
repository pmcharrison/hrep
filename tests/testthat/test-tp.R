context("tp")

library(magrittr)

test_that("example 1", {
  c1 <- pc_chord(c(8, 6, 9, 10))
  c2 <- tp(c1, 4)
  expect_equal(
    get_bass_pc(c2), 0
  )
  expect_equal(
    setdiff(as.numeric(pc_set(c2)),
            get_bass_pc(c2)),
    c(1, 2, 10)
  )
})

test_that("reversible", {
  n <- 10
  for (i in seq_len(10)) {
    c1 <- pc_chord(sample(11, 5))
    int <- sample(-11:11, 1)
    c2 <- tp(c1, int)
    c3 <- tp(c2, -int)
    expect_equal(c1, c3)
  }
})

test_that("pc_set", {
  p1 <- pc_set(c(0, 4, 7))
  expect_is(tp(p1, 2), "pc_set")
  expect_equal(
    tp(p1, 2) %>% as.integer,
    c(2, 6, 9)
  )
  expect_equal(
    c(0, 4, 7) %>% pc_set %>% tp(-2) %>% as.integer,
    c(2, 5, 10)
  )
})

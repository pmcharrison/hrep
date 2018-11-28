context("test-int_vec")

test_that("format", {
  x <- int_vec(pc_set(c(0, 1, 5)))
  expect_is(x, "integer")
  expect_is(x, "int_vec")
  expect_true(length(x) == 6L)
})

test_that("coercion", {
  expect_equal(
    int_vec(c(0, 1, 5)),
    int_vec(pc_set(c(0, 1, 5))),
  )
  expect_equal(
    int_vec(c(60, 61, 65)),
    int_vec(c(0, 1, 5))
  )
})

test_that("transposition invariance", {
  N <- 20
  n <- 5
  for (i in seq_len(N)) {
    x1 <- pc_set(sample(0:11, size = n, replace = FALSE))
    x2 <- tp(x1, sample(1:11, size = 1))
    expect_equal(int_vec(x1), int_vec(x2))
  }
})

test_that("examples", {
  expect_equal(int_vec(c(0, 1, 5)), .int_vec(c(1, 0, 0, 1, 1, 0)))
  expect_equal(int_vec(c(0, 1, 2)), .int_vec(c(2, 1, 0, 0, 0, 0)))
  expect_equal(int_vec(c(0, 4, 7)), .int_vec(c(0, 0, 1, 1, 1, 0)))
  expect_equal(int_vec(c(0, 1, 4, 6)), .int_vec(c(1, 1, 1, 1, 1, 1)))
  expect_equal(int_vec(c(0, 1, 3, 7)), .int_vec(c(1, 1, 1, 1, 1, 1)))
  expect_equal(int_vec(c(3, 9, 0, 11, 4, 2)), .int_vec(c(3, 3, 3, 2, 3, 1)))
})

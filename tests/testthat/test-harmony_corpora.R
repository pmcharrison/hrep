context("harmony_corpora")

test_that("examples", {
  x1 <- sample(1:100, 20, replace = TRUE)
  x2 <- sample(1:100, 25, replace = TRUE)

  c1 <- as.harmony_composition(x1)
  c2 <- as.harmony_composition(x2)

  expect_equal(
    num_symbols(c1), 20
  )
  expect_equal(
    num_symbols(c2), 25
  )

  d <- as.harmony_corpus(list(c1, c2))

  expect_equal(
    num_symbols(d), 45
  )

  expect_equal(
    d, as.harmony_corpus(list(x1, x2))
  )

  # expect_equal(
  #   declass(c1),
  #   as.integer(c1)
  # )

  expect_equal(
    as.integer(c1),
    x1
  )

  # expect_equal(
  #   declass(d),
  #   list(x1, x2)
  # )
})

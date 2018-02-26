context("combine_corpora")

test_that("example", {
  x1 <- sample(1:1000, 20)
  x2 <- sample(1:1000, 20)
  x3 <- sample(1:1000, 20)
  x4 <- sample(1:1000, 20)

  c1 <- as.harmony_corpus(list(x1, x2))
  c2 <- as.harmony_corpus(list(x3, x4))
  c3 <- as.harmony_corpus(list(x1, x2, x3, x4))
  c4 <- combine_corpora(c1, c2)

  expect_equal(c3, c4)
})

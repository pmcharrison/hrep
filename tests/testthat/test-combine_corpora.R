context("combine_corpora")

test_that("example", {
  x1 <- sample(1:1000, 10) %>% coded_vec("x")
  x2 <- sample(1:1000, 20) %>% coded_vec("x")
  x3 <- sample(1:1000, 30) %>% coded_vec("x")
  x4 <- sample(1:1000, 40) %>% coded_vec("x")

  c1 <- corpus(list(x1, x2), type = "x")
  c2 <- corpus(list(x3, x4), type = "x")
  c3 <- corpus(list(x1, x2, x3, x4), type = "x")
  c4 <- c(c1, c2)

  expect_equal(c3, c4)
})

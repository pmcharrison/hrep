context("corpus")

test_that("examples", {
  x1 <- sample(1:100, 20, replace = TRUE)
  x2 <- sample(1:100, 25, replace = TRUE)

  c1 <- coded_vec(x1, "x")
  c2 <- coded_vec(x2, "x")

  expect_equal(
    num_elements(c1), 20
  )
  expect_equal(
    num_elements(c2), 25
  )

  d <- corpus(list(c1, c2), type = "x")

  expect_equal(
    num_elements(d), 45
  )

  expect_equal(
    as.integer(c1),
    x1
  )
})

test_that("more examples", {
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

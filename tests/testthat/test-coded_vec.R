context("test-coded_vec")

test_that("indexing", {
  x <- coded_vec(1:5, type = "pc_set")
  expect_equal(x[1:3],
               coded_vec(1:3, type = "pc_set"))
  expect_equal(x[[1]], 1)
  x[1:3] <- 11:13
  expect_equal(x, coded_vec(c(11, 12, 13, 4, 5), type = "pc_set"))
  x[[5]] <- 50
  expect_equal(x, coded_vec(c(11, 12, 13, 4, 50), type = "pc_set"))
})

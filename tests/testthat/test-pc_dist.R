context("pc_dist")

test_that("examples", {
  expect_equal(pc_dist(0, 3), 3)
  expect_equal(pc_dist(11, 0), 1)
  expect_equal(pc_dist(7, 1), 6)
  expect_equal(pc_dist(7, 9), 2)
  expect_equal(pc_dist(2, 4), 2)
  expect_equal(pc_dist(1, 11), 2)
  expect_equal(pc_dist(
    c(0, 11, 7, 7, 2, 1),
    c(3, 0, 1, 9, 4, 11)
  ), c(3, 1, 6, 2, 2, 2))
})

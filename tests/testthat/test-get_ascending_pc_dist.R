context("get_ascending_pc_dist")

test_that("examples", {
  expect_equal(
    get_ascending_pc_dist(1, 5),
    4
  )
  expect_equal(
    get_ascending_pc_dist(7, 10),
    3
  )
  expect_equal(
    get_ascending_pc_dist(11, 0),
    1
  )
  expect_equal(
    get_ascending_pc_dist(7, 1),
    6
  )
})

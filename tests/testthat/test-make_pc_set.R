context("new_pc_set")

test_that("various", {
  expect_is(
    new_pc_set(c(0, 4, 7)),
    "pc_set"
  )
  expect_equal(
    new_pc_set(c(0, 4, 7)),
    new_pc_set(c(4, 7, 0))
  )
  expect_error(
    new_pc_set("cat")
  )
  expect_error(
    new_pc_set(12)
  )
  expect_error(
    new_pc_set(c(0, 2, 2))
  )
  expect_equal(
    as.integer(new_pc_set(c(7, 3, 2))),
    c(2, 3, 7)
  )
  expect_is(
    as.integer(new_pc_set(c(5, 6, 7))),
    "integer"
  )
  expect_is(
    as.numeric(new_pc_set(c(5, 6, 7))),
    "numeric"
  )
})

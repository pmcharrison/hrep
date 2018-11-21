context("get_pc_set_storage_key")

test_that("examples", {
  expect_equal(
    get_pc_set_storage_key(c(0, 4, 7)),
    "0 4 7"
  )
  expect_equal(
    get_pc_set_storage_key(pc_set(c(0, 4, 7))),
    "0 4 7"
  )
})

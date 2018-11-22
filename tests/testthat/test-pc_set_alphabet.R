context("get_pc_set_alphabet")

x <- pc_set_alphabet$by_id

test_that("class", {
  expect_is(x, "list")
  expect_true(
    all(sapply(x, is.integer))
  )
})

test_that("size", {
  expect_equal(
    length(x), 2 ^ 12 - 1
  )
})

test_that("duplicates", {
  expect_true(
    !anyDuplicated(x)
  )
})

test_that("example", {
  expect_true(
    length(Filter(
      function(y) all.equal(y, pc_set(c(0, 1, 2, 3))),
      x
    )) == 1
  )
})

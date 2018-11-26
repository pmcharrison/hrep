context("pc_set_alphabet")

x <- pc_set_alphabet$by_id

test_that("class", {
  expect_is(x, "list")
  expect_true(
    all(sapply(x, is.pc_set))
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

test_that("consistency with previous versions", {
  old <- new.env()
  load(system.file("stability-tests/pc_set_alphabet.rda",
                   package = "hrep",
                   mustWork = TRUE),
       envir = old)
  expect_equal(
    pc_set_alphabet,
    old$pc_set_alphabet
  )
})

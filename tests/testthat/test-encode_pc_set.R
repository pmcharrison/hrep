context("encode_pc_set")

test_that("format", {
  expect_is(encode(pc_set(0)), "integer")
})

test_that("encoding and decoding pc-sets", {
  expect_equal(encode(pc_set(0)), 1)
  rand <- sample(4e3, 20)
  expect_equal(
    rand,
    as.integer(encode(decode(rand, type = "pc_set")))
  )
})

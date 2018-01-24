context("encode_pc_set")

test_that("format", {
  expect_is(encode_pc_set(0),
            "integer")
})

test_that("encoding and decoding pc-sets", {
  expect_equal(encode_pc_set(0), 1)
  rand <- sample(4e3, 20)
  expect_equal(
    rand,
    encode_pc_sets(decode_pc_sets(rand))
  )
})

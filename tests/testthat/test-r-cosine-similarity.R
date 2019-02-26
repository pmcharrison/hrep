context("test-r-cosine-similarity")

test_that("examples", {
  expect_equal(cosine_similarity(rep(1, times = 10),
                                 rep(2, times = 10)),
               1)
  expect_equal(cosine_similarity(rep(1, times = 10),
                                 rep(-2, times = 10)),
               -1)
  expect_equal(
    cosine_similarity(
      c(0, 3, 0, 0, 2, 0, 0, 2, 0, 5),
      c(1, 2, 0, 0, 1, 1, 0, 1, 0, 3)
    ),
    0.94,
    tolerance = 1e-2
  )
  expect_equal(
    cosine_similarity(
      c(0, 3, 0, 0, 2, 0, 0, 2, 0, 5),
      c(1, 2, 0, 0, 1, 1, 0, 1, 0, 3)
    ),
    0.9356015
  )
})

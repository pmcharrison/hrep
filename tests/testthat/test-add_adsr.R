test_that("basic example", {
  x <- .wave(rep(10, times = 20), sample_rate = 1)
  y <- adsr_filter(
    x,
    attack = 3,
    decay = 2,
    sustain = 0.7,
    hold = 5,
    release = 2
  )
  expect_equal(
    as.numeric(y),
    c(
      0, 10/3, 20/3, 10,
      8.5,
      7, 7, 7, 7, 7, 7,
      3.5,
      0, 0, 0, 0, 0, 0, 0, 0
    )
  )
})

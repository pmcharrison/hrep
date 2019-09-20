test_that("misc", {
  data.frame(x = 1:5, y = 1) %>%
    list() %>%
    collapse_summing_amplitudes(digits = 6) %>%
    expect_equal(data.frame(x = 1:5, y = 1))

  list(
    data.frame(x = 1:5, y = 1),
    data.frame(x = 1:5, y = 1)
  ) %>%
    collapse_summing_amplitudes(digits = 6) %>%
    expect_equal(data.frame(x = 1:5, y = sqrt(2)))

  list(
    data.frame(x = 1:5, y = 1),
    data.frame(x = 1:5 + 1e-9, y = 1)
  ) %>%
    collapse_summing_amplitudes(digits = 6) %>%
    expect_equal(data.frame(x = 1:5, y = sqrt(2)))

  list(
    data.frame(x = 1:5, y = 1),
    data.frame(x = 1:5 + 0.1, y = 1)
  ) %>%
    collapse_summing_amplitudes(digits = 0) %>%
    expect_equal(data.frame(x = 1:5, y = sqrt(2)))

  list(
    data.frame(x = 1:5, y = 1),
    data.frame(x = 1:5 + 0.1, y = 1)
  ) %>%
    collapse_summing_amplitudes(digits = 3) %>%
    expect_equal(data.frame(x = rep(1:5, each = 2) + rep(c(0, 0.1), times = 5),
                            y = 1))
})

test_that("modulo", {
  list(data.frame(x = 0:12, y = 1)) %>%
    collapse_summing_amplitudes(digits = 0, modulo = 12) %>%
    expect_equal(data.frame(x = 0:11,
                            y = c(sqrt(2), rep(1, times = 11))))

  list(data.frame(x = 0:23, y = 1)) %>%
    collapse_summing_amplitudes(digits = 0, modulo = 12) %>%
    expect_equal(data.frame(x = 0:11,
                            y = sqrt(2)))
})

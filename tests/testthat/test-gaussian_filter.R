test_that("worked example", {
  ref_density <- dnorm(1) / dnorm(0)

  fr_chord(100) %>%
    gaussian_filter(location = c(100, 1000, 2000),
                   width = 100,
                   roll_off = 0, num_harmonics = 30) %>%
    tibble::as_tibble() %>%
    (function(x) x[x$y > 0.2, ]) %>%
    expect_equal(tibble::tribble(
      ~ x,   ~ y,
      100,  1,
      200,  ref_density,
      900,  ref_density,
      1000, 1,
      1100, ref_density,
      1900, ref_density,
      2000, 1,
      2100, ref_density
    ), check.attributes = FALSE, tolerance = 1e-5)
})

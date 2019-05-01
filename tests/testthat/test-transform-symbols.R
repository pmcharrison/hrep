context("test-transform-symbols")

test_that("argument propagation", {
  list(
    pi_chord(c(60, 64, 67)),
    pi_chord(c(59, 62, 67)),
    pi_chord(c(60, 64, 67))
  ) %>%
    vec("pi_chord") %>%
    transform_symbols(milne_pc_spectrum, "milne_pc_spectrum", array_dim = 100) %>%
    purrr::map_int(length) %>%
    unique %>%
    expect_equal(100)
})

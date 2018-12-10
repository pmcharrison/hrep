context("test-expand_harmonics")

test_that("misc", {
  .fr_sparse_spectrum(frequency = c(1, 2),
                      amplitude = c(1, 1)) %>%
    expand_harmonics(num_harmonics = 1) %T>%
    expect_is("fr_sparse_spectrum") %>%
    expect_equal(data.frame(x = c(1, 2),
                            y = c(1, 1)),
                 check.attributes = FALSE)

  .fr_sparse_spectrum(frequency = c(1, 2),
                      amplitude = c(1, 1)) %>%
    expand_harmonics(num_harmonics = 3) %T>%
    expect_is("fr_sparse_spectrum") %>%
    expect_equal(data.frame(x = c(1, 2, 3, 4, 6),
                            y = c(1, sqrt(1.25), 1 / 3, 1 / 2, 1 / 3)),
                 check.attributes = FALSE)

  pi_chord(0) %>%
    expand_harmonics(num_harmonics = 6) %>%
    expect_equal(data.frame(x = c(0, 12, 19, 24, 28, 31),
                            y = c(1, 1/2, 1/3, 1/4, 1/5, 1/6)),
                 check.attributes = FALSE,
                 tolerance = 1e-1)
})

test_that("roll-off", {
  pi_chord(0) %>%
    expand_harmonics(num_harmonics = 6, roll_off = 2) %>%
    {amp(.)} %>%
    expect_equal(c(1, 1 / 2 ^ 2, 1 / 3 ^ 2, 1 / 4 ^ 2, 1 / 5 ^ 2, 1 / 6 ^ 2))
})

test_that("MIDI transposition", {
  pi_chord(0) %>%
    expand_harmonics(num_harmonics = 6, roll_off = 2) %>%
    {pitch(.)} %>%
    expect_equal(c(10, 22, 29, 34, 38, 41),
                 tolerance = 1)
})

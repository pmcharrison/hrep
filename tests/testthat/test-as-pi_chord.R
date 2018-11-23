context("test-as-pi_chord")

test_that("pc_chord", {
  pc_chord(0, c(4, 7)) %>%
    as.pi_chord() %>%
    as.numeric() %>%
    expect_equal(c(48, 64, 67))

  pc_chord(4, c(0, 7)) %>%
    as.pi_chord() %>%
    as.numeric() %>%
    expect_equal(c(52, 60, 67))
})

test_that("pc_set", {
  pc_set(c(0, 4, 7)) %>%
    as.pi_chord() %>%
    as.numeric() %>%
    expect_equal(c(60, 64, 67))

  pc_set(c(2, 4, 7)) %>%
    as.pi_chord() %>%
    as.numeric() %>%
    expect_equal(c(62, 64, 67))
})

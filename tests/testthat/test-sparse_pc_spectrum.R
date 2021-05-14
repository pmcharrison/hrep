test_that("misc", {
  sparse_pc_spectrum(c(0, 4, 7), num_harmonics = 1) %>%
    as.data.frame() %>%
    expect_equal(data.frame(x = c(0, 4, 7),
                            y = 1))

  sparse_pc_spectrum(c(0, 4, 7), num_harmonics = 3, digits = 0) %>%
    as.data.frame() %>%
    expect_equal(data.frame(x = c(0, 2, 4, 7, 11),
                            y = c(sum_amplitudes(1, 0.5),
                                  1/3,
                                  sum_amplitudes(1, 0.5),
                                  sum_amplitudes(1, 0.5) %>% sum_amplitudes(1/3),
                                  1/3)))

  expect_equal(
    sparse_pc_spectrum(c(0, 4, 7)),
    sparse_pi_spectrum(c(60, 64, 67)) %>% sparse_pc_spectrum()
  )

  expect_equal(
    sparse_pc_spectrum(c(0, 4, 7)),
    sparse_pi_spectrum(c(60, 64, 67)) %>% sparse_fr_spectrum() %>% sparse_pc_spectrum()
  )
})

test_that("coherent", {
  expect_equal(
    sparse_pc_spectrum(pi_chord(c(60, 60.000001)), coherent = TRUE, num_harmonics = 2) %>% as.data.frame() %>% `$`(y),
    c(1.5, 1.5) # the partials are still separated
  )
  expect_equal(
    sparse_pc_spectrum(pi_chord(c(60, 60.0000001)), coherent = TRUE, num_harmonics = 2) %>% as.data.frame() %>% `$`(y),
    c(3) # we are now within the rounding threshold, the partials now add coherently
  )
  expect_equal(
    sparse_pc_spectrum(pi_chord(c(60, 60.0000001)), coherent = FALSE, num_harmonics = 1) %>% as.data.frame() %>% `$`(y),
    sqrt(2) # testing incoherent addition, this time with just one harmonic
  )
})

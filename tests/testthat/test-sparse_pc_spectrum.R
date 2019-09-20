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

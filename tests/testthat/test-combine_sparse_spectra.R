test_that("replicating collapse_summing_amplitudes tests", {
  x <- sparse_pi_spectrum(list(pitch = 1:5, amplitude = rep(1, times = 5)))
  expect_equal(combine_sparse_spectra(x), x)

  x <- combine_sparse_spectra(
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    digits = 6
  )
  expect_is(x, "sparse_pi_spectrum")
  expect_equal(as.data.frame(x),
               data.frame(x = 1:5, y = sqrt(2)))

  combine_sparse_spectra(
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    .sparse_pi_spectrum(pitch = 1:5 + 1e-9, amp = rep(1, times = 5)),
    digits = 6
  ) %>%
    as.data.frame() %>%
    expect_equal(data.frame(x = 1:5, y = sqrt(2)))

  combine_sparse_spectra(
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    .sparse_pi_spectrum(pitch = 1:5 + 0.1, amp = rep(1, times = 5)),
    digits = 0
  ) %>%
    as.data.frame() %>%
    expect_equal(data.frame(x = 1:5, y = sqrt(2)))

  combine_sparse_spectra(
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    .sparse_pi_spectrum(pitch = 1:5 + 0.1, amp = rep(1, times = 5)),
    digits = 3
  ) %>%
    as.data.frame() %>%
    expect_equal(data.frame(x = rep(1:5, each = 2) + rep(c(0, 0.1), times = 5),
                            y = 1))
})

test_that("combining heterogeneous spectra", {
  x <- combine_sparse_spectra(
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5)),
    .sparse_fr_spectrum(frequency = 1:5, amp = rep(1, times = 5))
  )
  expect_is(x, "sparse_pi_spectrum")
  expect_equal(
    as.data.frame(x),
    data.frame(x = c(freq_to_midi(1:5), 1:5) %>% round(6),
               y = 1)
  )

  x <- combine_sparse_spectra(
    .sparse_fr_spectrum(frequency = 1:5, amp = rep(1, times = 5)),
    .sparse_pi_spectrum(pitch = 1:5, amp = rep(1, times = 5))
  )
  expect_is(x, "sparse_fr_spectrum")
  expect_equal(
    as.data.frame(x),
    data.frame(x = c(1:5 %>% freq_to_midi() %>% round(6) %>% midi_to_freq(),
                     midi_to_freq(1:5)),
               y = 1)
  )

})

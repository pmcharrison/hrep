context("test-sparse_fr_spectrum")

test_that('octave ratio stretches and compresses',{
  harmonic_default_result <- 60 %>%
    sparse_fr_spectrum(num_harmonics=10) %>%
    freq %>% freq_to_midi
  expect_equal(harmonic_default_result[1], 60)
  expect_equal(harmonic_default_result[2], 60+12)
  expect_equal(harmonic_default_result[4], 60+24)
  expect_equal(harmonic_default_result[8], 60+36)

  harmonic_explicit_result <- 60 %>%
    sparse_fr_spectrum(num_harmonics=10, octave_ratio = 2.0) %>%
    freq %>% freq_to_midi
  expect_equal(harmonic_explicit_result[1], 60)
  expect_equal(harmonic_explicit_result[2], 60+12)
  expect_equal(harmonic_explicit_result[4], 60+24)
  expect_equal(harmonic_explicit_result[8], 60+36)

  compressed_result <- 60 %>%
    sparse_fr_spectrum(num_harmonics=10, octave_ratio = 1.9) %>%
    freq %>% freq_to_midi
  expect_equal(compressed_result[1], 60)
  expect_equal(compressed_result[2],
               freq_to_midi(midi_to_freq(60)*1.9^log2(2)))
  expect_equal(compressed_result[4],
               freq_to_midi(midi_to_freq(60)*1.9^log2(4)))
  expect_equal(compressed_result[8],
               freq_to_midi(midi_to_freq(60)*1.9^log2(8)))

  stretcheded_result <- 60 %>%
    sparse_fr_spectrum(num_harmonics=10, octave_ratio = 2.1) %>%
    freq %>% freq_to_midi
  expect_equal(stretcheded_result[1], 60)
  expect_equal(stretcheded_result[2],
               freq_to_midi(midi_to_freq(60)*2.1^log2(2)))
  expect_equal(stretcheded_result[4],
               freq_to_midi(midi_to_freq(60)*2.1^log2(4)))
  expect_equal(stretcheded_result[8],
               freq_to_midi(midi_to_freq(60)*2.1^log2(8)))

})

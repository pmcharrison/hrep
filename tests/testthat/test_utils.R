context("utils")

test_that("midi_to_freq", {
  expect_equal(
    midi_to_freq(69),
    440
  )
  expect_equal(
    midi_to_freq(60) %>% round(digits = 1),
    261.6
  )
  expect_equal(
    midi_to_freq(21) %>% round(digits = 1),
    27.5
  )
  expect_equal(
    midi_to_freq(69, stretched_octave = FALSE),
    440)
  expect_equal(midi_to_freq(81, stretched_octave = FALSE),
               880)
  expect_equal(midi_to_freq(c(69, 81), stretched_octave = FALSE),
               c(440, 880))
})

test_that("freq_to_midi", {
  expect_equal(
    freq_to_midi(440),
    69
  )
  expect_equal(
    freq_to_midi(261.6) %>% round(digits = 1),
    60
  )
  expect_equal(
    freq_to_midi(880),
    81
  )
  expect_equal(
    freq_to_midi(880, stretched_octave = TRUE),
    80.9
  )
  rand <- rnorm(n = 100, mean = 60, sd = 40)
  expect_equal(
    rand %>% midi_to_freq %>% freq_to_midi,
    rand
  )
  expect_false(
    (rand %>% midi_to_freq(stretched_octave = TRUE) %>% freq_to_midi == rand) %>% all
  )
  expect_equal(
    rand %>% midi_to_freq(stretched_octave = TRUE) %>%
      freq_to_midi(stretched_octave = TRUE),
    rand
  )
})

test_that("env_to_df", {
  env <- new.env()
  env$cat <- 1
  env$dog <- 2
  expect_equal(
    env_to_df(env, decreasing = FALSE),
    data.frame(
      key = c("cat", "dog"),
      value = c(1, 2),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    env_to_df(env, decreasing = TRUE),
    data.frame(
      key = c("dog", "cat"),
      value = c(2, 1),
      stringsAsFactors = FALSE
    )
  )
})

test_that("reduce_by_key", {
  expect_equal(
    reduce_by_key(keys = c("a", "a", "a", "b", "b"),
                          values = c(1, 1, 1, -1, -1),
                          function(x, y) x + y),
    data.frame(
      key = c("a", "b"),
      value = c(3, -2),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    reduce_by_key(keys = c("a", "b", "a", "b", "a"),
                          values = c(1, -1, 1, -1, 1),
                          function(x, y) x + y),
    data.frame(
      key = c("a", "b"),
      value = c(3, -2),
      stringsAsFactors = FALSE
    )
  )
})

test_that("amplitude_to_dB", {
  expect_equal(
    amplitude_to_dB(
      amplitude = 1, unit_amplitude_in_dB = 60
    ),
    60,
    check.attributes = FALSE
  )
  expect_equal(
    amplitude_to_dB(
      amplitude = 1, unit_amplitude_in_dB = 30
    ),
    30,
    check.attributes = FALSE
  )
  expect_equal(
    amplitude_to_dB(
      amplitude = 10, unit_amplitude_in_dB = 30
    ),
    50,
    check.attributes = FALSE
  )
  expect_equal(
    amplitude_to_dB(
      amplitude = 100, unit_amplitude_in_dB = 30
    ),
    70,
    check.attributes = FALSE
  )
})

test_that("dB_to_amplitude", {
  expect_equal(
    dB_to_amplitude(
      dB = 60, unit_amplitude_in_dB = 60
    ),
    1,
    check.attributes = FALSE
  )
  expect_equal(
    dB_to_amplitude(
      dB = 20, unit_amplitude_in_dB = 60
    ),
    0.01,
    check.attributes = FALSE
  )
  expect_equal(
    dB_to_amplitude(
      dB = 100, unit_amplitude_in_dB = 60
    ),
    100,
    check.attributes = FALSE
  )
})

test_that("sum_amplitudes", {
  expect_equal(
    sum_amplitudes(1, 1, coherent = TRUE, dB = FALSE),
    2
  )
  expect_equal(
    sum_amplitudes(60, 60, coherent = FALSE, dB = TRUE) %>% round(digits = 2),
    63.01
  )
  expect_equal(
    sum_amplitudes(60, 70, coherent = FALSE, dB = TRUE) %>% round(digits = 2),
    70.41
  )
  expect_equal(
    sum_amplitudes(1, 1, coherent = FALSE, dB = FALSE),
    sqrt(2)
  )
  expect_equal(
    sum_amplitudes(60, 60, coherent = TRUE, dB = TRUE),
    amplitude_to_dB(2, unit_amplitude_in_dB = 60),
    check.attributes = FALSE
  )
})

test_that("pi_to_pc", {
  expect_equal(
    pi_to_pc(26),
    2
  )
  expect_equal(
    pi_to_pc(72),
    0
  )
  expect_equal(
    pi_to_pc(c(26, 72, 5)),
    c(2, 0, 5)
  )
})

context("utils")

test_that("expand_harmonics", {
  expect_equal(
    expand_harmonics(
      frequency = c(1, 2),
      amplitude = rep(1, times = 2),
      num_harmonics = 1
    ),
    data.frame(
      frequency = c(1, 2),
      amplitude = c(1, 1)
    )
  )
  expect_equal(
    expand_harmonics(
      frequency = c(1, 2),
      amplitude = rep(1, times = 2),
      num_harmonics = 3
    ),
    data.frame(
      frequency = c(1, 2, 3, 4, 6),
      amplitude = c(1, sqrt(1.25), 1 / 3, 1 / 2, 1 / 3)
    )
  )
})

test_that("convert_midi_to_freq", {
  expect_equal(
    convert_midi_to_freq(69),
    440
  )
  expect_equal(
    convert_midi_to_freq(60) %>% round(digits = 1),
    261.6
  )
  expect_equal(
    convert_midi_to_freq(21) %>% round(digits = 1),
    27.5
  )
})

test_that("convert_env_to_df", {
  env <- new.env()
  env$cat <- 1
  env$dog <- 2
  expect_equal(
    convert_env_to_df(env, decreasing = FALSE),
    data.frame(
      key = c("cat", "dog"),
      value = c(1, 2),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    convert_env_to_df(env, decreasing = TRUE),
    data.frame(
      key = c("dog", "cat"),
      value = c(2, 1),
      stringsAsFactors = FALSE
    )
  )
})

test_that("reduce_by_key", {
  expect_equal(
    HarmonyUtils:::reduce_by_key(keys = c("a", "a", "a", "b", "b"),
                                 values = c(1, 1, 1, -1, -1),
                                 function(x, y) x + y),
    data.frame(
      key = c("a", "b"),
      value = c(3, -2),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    HarmonyUtils:::reduce_by_key(keys = c("a", "b", "a", "b", "a"),
                                 values = c(1, -1, 1, -1, 1),
                                 function(x, y) x + y),
    data.frame(
      key = c("a", "b"),
      value = c(3, -2),
      stringsAsFactors = FALSE
    )
  )
})

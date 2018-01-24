context("get_chord_alphabet_from_dataset")

test_that("format", {
  expect_is(
    get_chord_alphabet_from_dataset(HarmonyCorpora::classical),
    "integer"
    )
})

test_that("chord alphabet invariance to dataset order", {
  expect_equal(
    get_chord_alphabet_from_datasets(list(
      HarmonyCorpora::classical,
      HarmonyCorpora::jazz
    )),
    get_chord_alphabet_from_datasets(list(
      HarmonyCorpora::jazz,
      HarmonyCorpora::classical
    ))
  )
})

test_that("pitch-class set alphabet invariance to dataset order", {
  expect_equal(
    get_pc_set_alphabet_from_datasets(list(
      HarmonyCorpora::classical,
      HarmonyCorpora::jazz
    )),
    get_pc_set_alphabet_from_datasets(list(
      HarmonyCorpora::jazz,
      HarmonyCorpora::classical
    ))
  )
})

test_that("get_chord_alphabet_from_datasets equivalent to get_chord_alphabet_from_dataset", {
  expect_equal(
    get_chord_alphabet_from_dataset(HarmonyCorpora::classical),
    get_chord_alphabet_from_datasets(list(HarmonyCorpora::classical))
  )
})

test_that("get_pc_set_alphabet_from_datasets equivalent to get_pc_set_alphabet_from_dataset", {
  expect_equal(
    get_pc_set_alphabet_from_dataset(HarmonyCorpora::classical),
    get_pc_set_alphabet_from_datasets(list(HarmonyCorpora::classical))
  )
})

test_that("Approximate size of chord alphabets", {
  chord_alphabet <- get_chord_alphabet_from_datasets()
  expect_gt(length(chord_alphabet), 1000)
  expect_lt(length(chord_alphabet), 1500)
})

test_that("Approximate size of pitch-class set alphabets", {
  pc_alphabet <- get_pc_set_alphabet_from_datasets()
  expect_gt(length(pc_alphabet), 400)
  expect_lt(length(pc_alphabet), 600)
})

test_that("Format of alphabets", {
  chord_alphabet <- get_chord_alphabet_from_datasets()
  expect_true(is.numeric(chord_alphabet) || is.integer(chord_alphabet))

  pc_alphabet <- get_pc_set_alphabet_from_datasets()
  expect_true(all(sapply(pc_alphabet,
                         function(x) is.numeric(x) || is.integer(x))))

  chord_alphabet_decoded <- get_chord_alphabet_from_datasets(decode = TRUE)
  expect_true(all(sapply(chord_alphabet_decoded,
                         function(x) is.numeric(x) || is.integer(x))))
})

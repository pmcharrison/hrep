context("chord_alphabet")

test_that("chord_alphabet is consistent with previous code versions", {
  expect_equal(
    chord_alphabet$by_id,
    readRDS("chord_alphabet.rds")
  )
})

test_that("format", {
  chords <- sample(as.list(chord_alphabet$by_chord),
                   10)
  expect_true(
    all(sapply(chords, is.integer))
  )
}
)

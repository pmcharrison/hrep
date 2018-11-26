context("test-pc_chord_alphabet")

test_that("consistency with previous versions", {
  old <- new.env()
  load(system.file("stability-tests/pc_chord_alphabet.rda",
                   package = "hrep",
                   mustWork = TRUE),
       envir = old)
  expect_equal(
    pc_chord_alphabet,
    old$pc_chord_alphabet
  )
})

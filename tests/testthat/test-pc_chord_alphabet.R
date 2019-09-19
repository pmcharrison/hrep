context("test-pc_chord_alphabet")

test_that("misc", {
  expect_equal(encode_pc_chord(c(0, 3, 6)), 289)
  expect_equal(encode_pc_chord(c(0, 4, 7)), 145)
  expect_equal(encode_pc_chord(c(4, 0, 7)), 8457)
  expect_equal(encode_pc_chord(c(0)), 1)
  expect_equal(encode_pc_chord(c(0, 1)), 1025)

  pc_chord(c(0, 3, 6)) %>% encode()
})

test_that("consistency with previous versions", {
  old <- new.env()
  load(system.file("stability-tests/pc_chord_alphabet.rda",
                   package = "hrep",
                   mustWork = TRUE),
       envir = old)
  by_id <- pc_chord_alphabet$by_id
  for (i in seq_along(by_id)) {
    class(by_id[[i]]) <- "pc_chord"
  }
  expect_equal(
    by_id,
    old$pc_chord_alphabet$by_id
  )
  expect_equal(
    as.list(pc_chord_alphabet$by_pc_chord) %>%
      sapply(as.integer, simplify = FALSE) %>% unlist %>% sort,
    as.list(old$pc_chord_alphabet$by_pc_chord) %>% unlist %>% sort
  )
})

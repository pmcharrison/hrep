context("test-pc_chord_alphabet")

library(magrittr)

test_that("consistency with previous versions", {
  old <- new.env()
  load(system.file("stability-tests/pc_chord_alphabet.rda",
                   package = "hrep",
                   mustWork = TRUE),
       envir = old)
  expect_equal(
    pc_chord_alphabet$by_id,
    old$pc_chord_alphabet$by_id
  )
  expect_equal(
    as.list(pc_chord_alphabet$by_pc_chord) %>%
      sapply(as.integer, simplify = FALSE) %>% unlist %>% sort,
    as.list(old$pc_chord_alphabet$by_pc_chord) %>% unlist %>% sort
  )
})

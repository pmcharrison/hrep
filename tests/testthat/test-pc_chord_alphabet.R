context("test-pc_chord_alphabet")

library(magrittr)

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

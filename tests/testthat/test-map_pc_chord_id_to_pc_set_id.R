context("map_pc_chord_id_to_pc_set_id")

library(magrittr)

test_that("Format", {
  chord_ids <- c(3, 56, 80)
  pc_set_ids <- map_pc_chord_id_to_pc_set_id(chord_ids)
  expect_is(pc_set_ids, "integer")
  expect_equal(length(chord_ids),
               length(pc_set_ids))
})

test_that("Example results", {
  expect_equal(
    list(c(48, 64, 67), c(48, 63, 67)) %>%
      lapply(pi_chord) %>%
      lapply(pc_chord) %>%
      vec("pc_chord") %>%
      encode() %>%
      map_pc_chord_id_to_pc_set_id %>%
      decode("pc_set") %>%
      lapply(as.numeric),
    list(
      c(0, 4, 7),
      c(0, 3, 7)
    )
  )
})

test_that("treatment of NA values", {
  expect_equal(
    map_pc_chord_id_to_pc_set_id(NA),
    as.integer(NA)
  )
})

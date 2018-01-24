context("map_chord_id_to_pc_set_id")

test_that("Format", {
  chord_ids <- c(3, 56, 80)
  pc_set_ids <- map_chord_id_to_pc_set_id(chord_ids)
  expect_is(pc_set_ids, "integer")
  expect_equal(length(chord_ids),
               length(pc_set_ids))
})

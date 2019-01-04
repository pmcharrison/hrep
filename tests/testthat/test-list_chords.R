context("test-list_chords")

test_that("examples", {
  expect_equal(list_chords("pc_set"),
               pc_set_alphabet$by_id)
  expect_equal(list_chords("pc_chord_type") %>% map(as.integer),
               pc_chord_alphabet$by_id[seq_len(alphabet_size("pc_chord") / 12L)] %>%
                 map(as.integer))
})

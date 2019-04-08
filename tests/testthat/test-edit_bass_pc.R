context("test-edit_bass_pc")

test_that("examples", {
  pc_chord(c(0, 4, 7)) %>%
    edit_bass_pc(4) %>%
    expect_equal(pc_chord(c(4, 0, 7)))

  pc_chord(c(0, 4, 7)) %>%
    edit_bass_pc(7) %>%
    expect_equal(pc_chord(c(7, 0, 4)))

  pc_chord(c(0, 4, 7)) %>%
    edit_bass_pc(0) %>%
    expect_equal(pc_chord(c(0, 4, 7)))

  expect_error(
    pc_chord(c(0, 4, 7)) %>% edit_bass_pc(., 3),
    "requested bass pitch class was not found in original chord"
  )

  pc_chord(c(4, 2, 9)) %>%
    edit_bass_pc(2) %>%
    expect_equal(pc_chord(c(2, 4, 9)))
})

context("test-fr_chord")

test_that("examples", {
  expect_equal(fr_chord(pi_chord(c(60, 64, 67))),
               .fr_chord(midi_to_freq(c(60, 64, 67))))
  expect_equal(
    c(60, 64, 67) %>% pi_chord %>% fr_chord %>% pi_chord %>% as.numeric,
    c(60, 64, 67)
  )
  expect_equal(
    c(60, 63, 67) %>% pi_chord %>% fr_chord %>% pc_set %>% as.numeric,
    c(0, 3, 7)
  )
})

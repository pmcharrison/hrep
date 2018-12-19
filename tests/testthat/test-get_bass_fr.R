context("test-get_bass_fr")

test_that("examples", {
  60 %>% pi_chord %>% get_bass_fr %>% expect_equal(midi_to_freq(60))
  c(60, 70) %>% pi_chord %>% get_bass_fr %>% expect_equal(midi_to_freq(c(60)))
  50 %>% fr_chord %>% get_bass_fr %>% expect_equal(50)
})

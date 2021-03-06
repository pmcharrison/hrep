context("test-pi_chord_type")

test_that("examples", {
  pi_chord_type("60 64 67") %>% as.integer %>% expect_equal(c(0, 4, 7))
  pi_chord(c(3, 5, 9)) %>% pi_chord_type %>% expect_equal(.pi_chord_type(c(0, 2, 6)))

  "0 3 7" %>% pi_chord_type %>% pi_chord(force = TRUE) %>% as.numeric %>%
    expect_equal(c(60, 63, 67))
})

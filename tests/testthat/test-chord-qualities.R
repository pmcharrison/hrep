test_that("misc", {
  decode_chord_quality("1") %>% expect_equal(0)
  decode_chord_quality("dim") %>% expect_equal(c(0, 3, 6))
  decode_chord_quality("^") %>% expect_equal(c(0, 4, 7, 11))
  decode_chord_quality("h") %>% expect_equal(c(0, 3, 6, 10))
  decode_chord_quality("*7+*") %>% expect_equal(c(0, 1, 4, 6, 8, 10))

  decode_chord_quality("test") %>% expect_null()
  expect_error(decode_chord_quality("test", must_work = TRUE),
               "could not decode token: test")

  expect_error(decode_chord_quality(""),
               "the empty string is not a permissible chord label")

  register_chord_quality("test", c(0, 1, 2))
  decode_chord_quality("test") %>% expect_equal(c(0, 1, 2))

  expect_error(register_chord_quality("test", c(1, 2, 3)),
               "cannot insert duplicate chord quality: test")

  register_chord_quality("test", c(1, 2, 3), overwrite = TRUE)
  decode_chord_quality("test") %>% expect_equal(c(1, 2, 3))

  initialise_chord_qualities()
  decode_chord_quality("test") %>% expect_null()
})

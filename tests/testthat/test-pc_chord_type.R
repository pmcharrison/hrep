context("test-pc_chord_type")

library(magrittr)

test_that("examples", {
  c(0, 4, 7) %>% pc_chord_type %>% encode %>% decode("pc_chord_type") %>%
    extract2(1) %>%
    expect_equal(pc_chord_type(c(0, 4, 7)))

  c(60, 64, 67) %>% pi_chord %>% pc_chord_type %>%
    expect_equal(pc_chord_type(c(0, 4, 7)))

  c(7, 0, 4) %>% pc_chord %>% pc_chord_type %>%
    expect_equal(pc_chord_type(c(0, 5, 9)))

  c(7, 0, 4) %>% pc_chord_type %>%
    expect_equal(pc_chord_type(c(0, 4, 7)))

  c(0, 5, 7) %>% pc_chord_type %>% as.character %>%
    expect_equal("0 5 7")

  c(40, 53, 54) %>% pc_chord_type %>% as.numeric %>%
    expect_equal(c(0, 1, 2))
})

test_that("transposition", {
  c(0, 4, 7) %>% pc_chord_type %>% tp(7) %>%
    expect_equal(pc_chord(c(7, 2, 11)))
})

test_that("alphabet", {
  n <- pc_chord_type_alphabet_size()
  char <- character(n)
  for (i in seq_len(n)) {
    x <- decode(i, "pc_chord_type")[[1]]
    expect_is(x, "pc_chord_type")
    char[i] <- as.character(x)
  }
  expect_true(!anyDuplicated(char))
})

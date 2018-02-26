context("get_chord_counts")

test_that("toy example", {
  d <- as.harmony_corpus(list(
    a = c(1, 1, 2, 3, 1),
    b = c(3, 1, 4, 5)
  ))
  res <- get_chord_counts(d)
  expect_equal(names(res), as.character(1:5))
  expect_equal(as.numeric(res),
               c(4, 1, 2, 1, 1))
})

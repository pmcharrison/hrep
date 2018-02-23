context("cycle_vector")

test_that("examples", {
  expect_equal(
    cycle_vector(1:5),
    list(
      c(1, 2, 3, 4, 5),
      c(2, 3, 4, 5, 1),
      c(3, 4, 5, 1, 2),
      c(4, 5, 1, 2, 3),
      c(5, 1, 2, 3, 4)
    ) %>% do.call(rbind, .)
  )
  expect_equal(
    cycle_vector(numeric()),
    as.matrix(numeric())
  )
  expect_equal(
    cycle_vector(c("cat", "pig", "dog")),
    list(
      c("cat", "pig", "dog"),
      c("pig", "dog", "cat"),
      c("dog", "cat", "pig")
    ) %>% do.call(rbind, .))
})

context("test-represent")

library(magrittr)

test_that("examples", {
  x <- list(
    c(60, 64, 67),
    c(59, 62, 67),
    c(63, 67, 72)
  )
  y <- x[c(2, 3, 1)]

  x %>%
    represent("pi_chord") %T>%
    expect_equal(vec(x = purrr::map(x, pi_chord),
                     type = "pi_chord")) %>%
    represent("pc_set") %T>%
    expect_equal(vec(x = purrr::map(x, pc_set),
                     type = "pc_set"))

  list(x, y) %>%
    purrr::map(~ vec(purrr::map(., pi_chord),
                     "pi_chord")) %>%
    corpus("pi_chord") %>%
    represent("pc_set") %>%
    as.list %>%
    expect_equal(list(vec(x = purrr::map(x, pc_set), type = "pc_set"),
                      vec(x = purrr::map(y, pc_set), type = "pc_set")))
})

context("get_pc_set_normal_form")

library(magrittr)

test_that("examples", {
  expect_equal(
    get_pc_set_normal_form(c(0, 4, 7)) %>% as.integer,
    c(0, 4, 7)
  )
  expect_equal(
    get_pc_set_normal_form(c(3, 6, 7, 10)) %>% as.integer,
    c(0, 3, 4, 7)
  )
  stop("Implement test wrt website")
  expect_equal(
    get_pc_set_normal_form(c(0, 3, 6, 7, 8, 10)) %>% as.integer,
    c(5)
  )
})

test_that("transpositions of a pitch-class set all get the same normal form", {
  n <- 30
  for (i in seq_len(n)) {
    pc_set <- HarmonyUtils::decode_pc_sets(sample(4e3, 1))[[1]]
    expect_true(
      0:11 %>%
        lapply(function(x) transpose(pc_set, x)) %>%
        lapply(get_pc_set_normal_form) %>%
        lapply(as.integer) %>%
        unique %>%
        assertthat::is.scalar()
    )
  }
})

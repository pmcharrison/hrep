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

test_that("transpositions of a pitch-class set should all get the same normal form", {
  stop("implement me")
  stop("you need to implement a transpose pitch-class set method")
})

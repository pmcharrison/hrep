context("get_pc_set_normal_order")

library(magrittr)

test_that("various", {
  expect_equal(
    get_pc_set_normal_order(new_pc_set(c(8, 9, 3))) %>% as.integer,
    c(3, 8, 9)
  )
  expect_equal(
    get_pc_set_normal_order(new_pc_set(c(1, 5, 6, 8, 10))) %>% as.integer,
    c(5, 6, 8, 10, 1)
  )
  expect_equal(
    get_pc_set_normal_order(new_pc_set(c(0, 1, 2, 3, 9, 10, 11))) %>% as.integer,
    c(9, 10, 11, 0, 1, 2, 3)
  )
  expect_equal(
    get_pc_set_normal_order(c(0, 7, 4)) %>% as.integer,
    c(0, 4, 7)
  )
  expect_equal(
    get_pc_set_normal_order(c(0, 2, 10)) %>% as.integer,
    c(10, 0, 2)
  )
  expect_equal(
    get_pc_set_normal_order(new_pc_set(0)) %>% as.integer,
    0
  )
  expect_equal(
    get_pc_set_normal_order(new_pc_set(5)) %>% as.integer,
    5
  )
})

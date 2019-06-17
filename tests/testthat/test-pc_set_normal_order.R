context("pc_set_norm_order")

library(magrittr)

test_that("various", {
  expect_equal(
    pc_set_norm_order(pc_set(c(8, 9, 3))) %>% as.integer,
    c(3, 8, 9)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(1, 5, 6, 8, 10))) %>% as.integer,
    c(5, 6, 8, 10, 1)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 1, 2, 3, 9, 10, 11))) %>% as.integer,
    c(9, 10, 11, 0, 1, 2, 3)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 7, 4))) %>% as.integer,
    c(0, 4, 7)
  )
  expect_equal(
    pc_set_norm_order(c(0, 2, 10)) %>% as.integer,
    c(10, 0, 2)
  )
  expect_equal(
    pc_set_norm_order(pc_set(0)) %>% as.integer,
    0
  )
  expect_equal(
    pc_set_norm_order(pc_set(5)) %>% as.integer,
    5
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 6, 2, 11))) %>% as.integer,
    c(11, 0, 2, 6)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 3, 4, 5, 11))) %>% as.integer,
    c(11, 0, 3, 4, 5)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 3, 6, 9, 10, 11))) %>% as.integer,
    c(9, 10, 11, 0, 3, 6)
  )
  expect_equal(
    pc_set_norm_order(pc_set(c(0, 3, 4, 7, 8, 11))) %>% as.integer,
    c(3, 4, 7, 8, 11, 0)
  )
})

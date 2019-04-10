context("test-vec")

test_that("indexing", {
  x <- list(
    pc_set(1),
    pc_set(2),
    pc_set(3)
  ) %>% vec("pc_set", metadata = list("meta"))

  expect_equal(x[1:2],
               vec(list(pc_set(1), pc_set(2)),
                   type = "pc_set",
                   metadata = list("meta")))
  expect_equal(x[[1]], pc_set(1))

  x[1:2] <- list(pc_set(10), pc_set(11))

  expect_equal(x, vec(list(pc_set(10), pc_set(11), pc_set(3)),
                      type = "pc_set",
                      metadata = list("meta")))
  x[[3]] <- pc_set(5)
  expect_equal(x, vec(list(pc_set(10), pc_set(11), pc_set(5)),
                      type = "pc_set",
                      metadata = list("meta")))
})

test_that("subclasses", {
  x <- c(0, 4, 7) %>% pc_chord %>% list() %>% vec("pc_chord")

  expect_equal(class(x),
               c("vec_pc_chord", "vec", "list"))

  x <- represent(x, "pc_set")

  expect_equal(class(x),
               c("vec_pc_set", "vec", "list"))
})

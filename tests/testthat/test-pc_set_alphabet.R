context("pc_set_alphabet")

x <- pc_set_alphabet$by_id

test_that("class", {
  expect_is(x, "list")
  expect_true(
    all(sapply(x, is.pc_set))
  )
})

test_that("size", {
  expect_equal(
    length(x), 2 ^ 12 - 1
  )
})

test_that("duplicates", {
  expect_true(
    !anyDuplicated(x)
  )
  expect_true(
    !anyDuplicated(names(pc_set_alphabet$by_chord))
  )
})

test_that("example", {
  expect_true(
    length(Filter(
      function(y) all.equal(y, pc_set(c(0, 1, 2, 3))),
      x
    )) == 1
  )
})

test_that("examples from paper", {
  11 %>% encode_pc_set() %>% expect_equal(1)
  11 %>% pc_set %>% encode() %>% as.integer() %>% expect_equal(1)

  10 %>% encode_pc_set() %>% expect_equal(2)
  10 %>% pc_set %>% encode() %>% as.integer() %>% expect_equal(2)

  c(10, 11) %>% encode_pc_set() %>% expect_equal(3)
  c(10, 11) %>% pc_set %>% encode() %>% as.integer() %>% expect_equal(3)

  c(0, 4, 7) %>% encode_pc_set() %>% expect_equal(2192)
  c(0, 4, 7) %>% pc_set %>% encode() %>% as.integer() %>% expect_equal(2192)
})

test_that("random consistency checks", {
  for (i in 1:20) {
    id <- sample(4095, 1)

    expect_equal(
      id %>% decode_pc_set() %>% list(),
      id %>% decode(x_type = "pc_set") %>% as.list()
    )

    expect_equal(
      id %>% decode_pc_set() %>% encode_pc_set(),
      id
    )
  }
})

test_that("consistency with previous versions", {
  if (FALSE)
    saveRDS(hrep::pc_set_alphabet, "inst/stability-tests/pc-set-alphabet.rds")

  old <- readRDS(system.file("stability-tests/pc-set-alphabet.rds",
                             package = "hrep",
                             mustWork = TRUE))

  expect_equal(old, pc_set_alphabet)
})

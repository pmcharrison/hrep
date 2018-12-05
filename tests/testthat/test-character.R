context("test-character")

test_that("examples", {
  expect_equal(pc_set("0 4 7"), pc_set(c(0, 4, 7)))
  expect_equal("0 4 7", "0 4 7" %>% pc_set %>% as.character)

  expect_equal(pc_chord("4 0 7"), pc_chord(c(4, 0, 7)))
  expect_equal("4 0 7", "4 0 7" %>% pc_chord %>% as.character)

  expect_equal(pi_chord("64 70 77"), pi_chord(c(64, 70, 77)))
  expect_equal("64 70 77", "64 70 77" %>% pi_chord %>% as.character)
})

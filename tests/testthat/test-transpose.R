context("transpose")

test_that("example 1", {
  c1 <- make_chord(8, c(6, 9, 10))
  c2 <- HarmonyUtils::transpose(c1, 4)
  expect_equal(
    HarmonyUtils::get_bass_pc(c2), 0
  )
  expect_equal(
    HarmonyUtils::get_non_bass_pc_set(c2), c(1, 2, 10)
  )
})

test_that("reversible", {
  n <- 10
  for (i in seq_len(10)) {
    c1 <- make_chord(sample(11, 1), sample(11, 4))
    int <- sample(-11:11, 1)
    c2 <- transpose(c1, int)
    c3 <- transpose(c2, -int)
    expect_equal(c1, c3)
  }
})

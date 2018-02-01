context("get_peak")

test_that("examples", {
  spec <- new("PCSpectrum", values = c(0, 0, 3, 4, 1))
  expect_equal(
    HarmonyUtils:::get_peak(spec),
    4
  )
})

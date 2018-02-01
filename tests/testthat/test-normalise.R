context("normalise")

test_that("examples", {
  spec <- new("PCSpectrum", values = c(0, 0, 3, 4, 1))
  norm <- HarmonyUtils:::normalise(spec)
  expect_is(norm, "PCSpectrum")
  bin_width <- 1 / length(spec@values)
  mass <- sum(0.2 * norm@values)
  expect_equal(mass, 1)

  spec_empty <- new("PCSpectrum", values = c(0, 0))
  expect_error(normalise(spec_empty))
})

context("cache_convert_pc_set_to_pc_spectrum")

x <- readRDS(system.file("extdata", "cache_convert_pc_set_to_pc_spectrum.rds",
                         package = "HarmonyUtils"))

test_that("type", {
  expect_is(
    x, "environment"
  )
})

test_that("size", {
  expect_equal(
    length(as.list(x)),
    2 ^ 12 - 1
  )
})

test_that("testing consistency of results", {
  pc_set <- c(0, 3, 7)
  expect_equal(
    HarmonyUtils::convert_pc_set_to_pc_spectrum(
      pc_set, cache = TRUE,
      cache_env = x,
      cache_stop_on_missing = TRUE
    ),
    HarmonyUtils::convert_pc_set_to_pc_spectrum(
      pc_set, cache = FALSE
    )
  )
})


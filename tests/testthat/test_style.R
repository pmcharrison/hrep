context("style")

test_that("style", {
  functions <- lsf.str("package:HarmonyUtils") %>%
    as.character
    # Remove any functions we wish to ignore
    # setdiff("get_complex_tone")
  sapply(functions, function(x) expect_true(checkFun(x)))
})

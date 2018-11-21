context("style")

test_that("style", {
  functions <- lsf.str("package:hutil") %>%
    as.character %>%
    # Remove any functions we wish to ignore
    setdiff("get_complex_tone")
  sapply(functions, function(x) expect_true(checkFun(x)))
})

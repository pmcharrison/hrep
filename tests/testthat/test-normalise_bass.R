context("normalise_bass")

test_that("examples", {
  expect_equal(
    HarmonyUtils::normalise_bass(HarmonyUtils::make_chord(4, c(7, 11))),
    HarmonyUtils::make_chord(0, c(3, 7))
  )
  expect_equal(
    HarmonyUtils::normalise_bass(HarmonyUtils::make_chord(10, c(4, 7))),
    HarmonyUtils::make_chord(0, c(6, 9))
  )
})

test_that("application to compositions", {
  c1 <- HarmonyUtils::make_chord(3, c(4, 5))
  c2 <- HarmonyUtils::make_chord(5, c(7, 9))
  c3 <- HarmonyUtils::make_chord(1, c(5, 8))
  c4 <- HarmonyUtils::make_chord(2, c(5, 9))
  expect_equal(
    HarmonyCorpora::as.Composition(list(HarmonyUtils::normalise_bass(c1),
                                        HarmonyUtils::normalise_bass(c2),
                                        HarmonyUtils::normalise_bass(c3),
                                        HarmonyUtils::normalise_bass(c4))),
    HarmonyUtils::normalise_bass(HarmonyCorpora::as.Composition(list(c1, c2, c3, c4)))
  )
})

test_that("application to corpora", {
  c1 <- HarmonyUtils::make_chord(3, c(4, 5))
  c2 <- HarmonyUtils::make_chord(5, c(7, 9))
  c3 <- HarmonyUtils::make_chord(1, c(5, 8))
  c4 <- HarmonyUtils::make_chord(2, c(5, 9))
  comp1 <- HarmonyCorpora::as.Composition(list(c1, c2))
  comp2 <- HarmonyCorpora::as.Composition(list(c3, c4))
  corp1 <- HarmonyCorpora::as.Corpus(list(comp1, comp2))
  expect_equal(
    HarmonyUtils::normalise_bass(corp1),
    HarmonyCorpora::as.Corpus(list(HarmonyUtils::normalise_bass(comp1),
                                   HarmonyUtils::normalise_bass(comp2)))
  )
})

context("normalise_bass")

test_that("examples", {
  expect_equal(
    hutil::normalise_bass(hutil::new_chord(4, c(7, 11))),
    hutil::new_chord(0, c(3, 7))
  )
  expect_equal(
    hutil::normalise_bass(hutil::new_chord(10, c(4, 7))),
    hutil::new_chord(0, c(6, 9))
  )
})

test_that("application to compositions", {
  c1 <- hutil::new_chord(3, c(4, 5))
  c2 <- hutil::new_chord(5, c(7, 9))
  c3 <- hutil::new_chord(1, c(5, 8))
  c4 <- hutil::new_chord(2, c(5, 9))
  expect_equal(
    as.harmony_composition(list(hutil::normalise_bass(c1),
                                hutil::normalise_bass(c2),
                                hutil::normalise_bass(c3),
                                hutil::normalise_bass(c4))),
    hutil::normalise_bass(as.harmony_composition(list(c1, c2, c3, c4)))
  )
})

test_that("application to corpora", {
  c1 <- hutil::new_chord(3, c(4, 5))
  c2 <- hutil::new_chord(5, c(7, 9))
  c3 <- hutil::new_chord(1, c(5, 8))
  c4 <- hutil::new_chord(2, c(5, 9))
  comp1 <- as.harmony_composition(list(c1, c2))
  comp2 <- as.harmony_composition(list(c3, c4))
  corp1 <- as.harmony_corpus(list(comp1, comp2))
  expect_equal(
    hutil::normalise_bass(corp1),
    as.harmony_corpus(list(hutil::normalise_bass(comp1),
                                   hutil::normalise_bass(comp2)))
  )
})

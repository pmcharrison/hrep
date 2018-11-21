context("normalise_bass")

test_that("examples", {
  expect_equal(
    normalise_bass(pc_chord(4, c(7, 11))),
    pc_chord(0, c(3, 7))
  )
  expect_equal(
    normalise_bass(pc_chord(10, c(4, 7))),
    pc_chord(0, c(6, 9))
  )
})

test_that("application to compositions", {
  c1 <- pc_chord(3, c(4, 5))
  c2 <- pc_chord(5, c(7, 9))
  c3 <- pc_chord(1, c(5, 8))
  c4 <- pc_chord(2, c(5, 9))
  expect_equal(
    as.harmony_composition(list(normalise_bass(c1),
                                normalise_bass(c2),
                                normalise_bass(c3),
                                normalise_bass(c4))),
    normalise_bass(as.harmony_composition(list(c1, c2, c3, c4)))
  )
})

test_that("application to corpora", {
  c1 <- pc_chord(3, c(4, 5))
  c2 <- pc_chord(5, c(7, 9))
  c3 <- pc_chord(1, c(5, 8))
  c4 <- pc_chord(2, c(5, 9))
  comp1 <- as.harmony_composition(list(c1, c2))
  comp2 <- as.harmony_composition(list(c3, c4))
  corp1 <- as.harmony_corpus(list(comp1, comp2))
  expect_equal(
    normalise_bass(corp1),
    as.harmony_corpus(list(normalise_bass(comp1),
                           normalise_bass(comp2)))
  )
})

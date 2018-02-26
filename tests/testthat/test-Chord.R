context("chord")

test_that("random examples", {
  chord <- HarmonyUtils::new_chord(2, c(4, 7))
  expect_equal(
    HarmonyUtils::get_bass_pc(chord), 2
  )
  expect_equal(
    HarmonyUtils::get_non_bass_pc_set(chord), c(4, 7)
  )
  expect_equal(
    HarmonyUtils::get_pc_set(chord),
    c(2, 4, 7)
  )
  expect_equal(
    HarmonyUtils::get_pc_set(new_chord(10, c(3, 4))),
    c(3, 4, 10)
  )
})

test_that("equivalences", {
  expect_equal(
    HarmonyUtils::new_chord(3, c(5, 7, 9)),
    HarmonyUtils::new_chord(3, c(9, 7, 5))
  )
  expect_equal(
    HarmonyUtils::new_chord(3, c(3, 5, 7, 9)),
    HarmonyUtils::new_chord(3, c(9, 7, 5))
  )
})

test_that("input checking", {
  expect_error(HarmonyUtils::new_chord(c(1, 2), 1:3))
  expect_error(HarmonyUtils::new_chord(c(12), 1:3))
  expect_error(HarmonyUtils::new_chord(1, 1:13))
  expect_error(HarmonyUtils::new_chord(2, c(3, 3)))
})

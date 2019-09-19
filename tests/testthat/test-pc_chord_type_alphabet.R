test_that("examples from paper", {
  c(0) %>% encode_pc_chord_type() %>% expect_equal(1)
  c(0) %>% pc_chord_type() %>% encode() %>% as.integer() %>% expect_equal(1)

  c(0, 1) %>% encode_pc_chord_type() %>% expect_equal(1025)
  c(0, 1) %>% pc_chord_type() %>% encode() %>% as.integer() %>% expect_equal(1025)

  c(0, 4, 7) %>% encode_pc_chord_type() %>% expect_equal(145)
  c(0, 4, 7) %>% pc_chord_type() %>% encode() %>% as.integer() %>% expect_equal(145)
})

test_that("consistency with pc_chord", {
  expect_equal(
    c(0, 9, 11) %>% encode_pc_chord_type(),
    c(0, 9, 11) %>% encode_pc_chord()
  )

  expect_equal(
    c(0, 1, 2) %>% encode_pc_chord_type(),
    c(0, 1, 2) %>% encode_pc_chord()
  )
})

test_that("checking for duplicates", {
  expect_true(!anyDuplicated(hrep::pc_chord_type_alphabet$by_id))
  expect_true(!anyDuplicated(names(hrep::pc_chord_type_alphabet$by_chord)))
})

test_that("consistency with previous versions", {
  if (FALSE)
    saveRDS(hrep::pc_chord_type_alphabet,
            "inst/stability-tests/pc-chord-type-alphabet.rds")

  old <- readRDS(system.file("stability-tests/pc-chord-type-alphabet.rds",
                             package = "hrep",
                             mustWork = TRUE))

  expect_equal(old, pc_chord_type_alphabet)
})

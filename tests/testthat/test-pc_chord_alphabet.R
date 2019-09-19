context("test-pc_chord_alphabet")

test_that("misc", {
  expect_equal(encode_pc_chord(c(0, 3, 6)), 289)
  expect_equal(encode_pc_chord(c(0, 4, 7)), 145)
  expect_equal(encode_pc_chord(c(4, 0, 7)), 8457)
  expect_equal(encode_pc_chord(c(0)), 1)
  expect_equal(encode_pc_chord(c(0, 1)), 1025)

  expect_equal(decode_pc_chord(2048), pc_chord(0:11))
  expect_equal(decode_pc_chord(2049), pc_chord(1))
  expect_equal(decode_pc_chord(2048 * 2), pc_chord(c(1, 0, 2:11)))

  pc_chord(c(0, 3, 6)) %>% encode() %>% as.integer() %>% expect_equal(289)

  list(
    pc_chord(c(0, 3, 6)),
    pc_chord(c(4, 0, 7))
  ) %>%
    vec("pc_chord") %>%
    encode() %>%
    as.integer() %>%
    expect_equal(c(289, 8457))

  pc_chord_alphabet$by_chord %>%
    names() %>%
    anyDuplicated() %>%
    expect_equal(0)

  pc_chord_alphabet$by_id %>% length() %>% expect_equal(24576)
  pc_chord_alphabet$by_chord %>% length() %>% expect_equal(24576)

  for (i in 1:20) {
    id <- sample(24576, 1)

    expect_equal(
      id %>% decode_pc_chord() %>% list(),
      id %>% decode(x_type = "pc_chord") %>% as.list()
    )

    expect_equal(
      id %>% decode_pc_chord() %>% encode_pc_chord(),
      id
    )
  }
})

test_that("consistency with previous versions", {
  if (FALSE)
    saveRDS(hrep::pc_chord_alphabet, "inst/stability-tests/pc-chord-alphabet.rds")

  old <- readRDS(system.file("stability-tests/pc-chord-alphabet.rds",
                             package = "hrep",
                             mustWork = TRUE))

  expect_equal(old, pc_chord_alphabet)
})

test_that("sanity checks", {
  expect_is(smooth_pc_spectrum("0 4 7"),
            "smooth_pc_spectrum")

  expect_equal(smooth_pc_spectrum("0 4 7"),
               smooth_pc_spectrum(pi_chord(c(0, 4, 7))))

  expect_equal(smooth_pc_spectrum(pi_chord(c(0, 4, 7))),
               smooth_pc_spectrum(pi_chord(c(0, 4, 7)) %>% fr_chord()))

})

test_that("regression tests", {
  if (FALSE) { # Saving reference outputs
    smooth_pc_spectrum(pi_chord(c(60, 64, 67))) %>% as.data.frame() %>%
      saveRDS("inst/stability-tests/smooth-pc-spectrum-60-64-67.rds")

    smooth_pc_spectrum(pi_chord(c(60, 64, 67)), num_harmonics = 2, sigma = 100) %>%
      as.data.frame() %>%
      saveRDS("inst/stability-tests/smooth-pc-spectrum-60-64-67-v2.rds")
  }

  expect_equal(smooth_pc_spectrum(pi_chord(c(60, 64, 67))) %>% as.data.frame(),
               readRDS(system.file("stability-tests/smooth-pc-spectrum-60-64-67.rds",
                                   package = "hrep")))

  expect_equal(smooth_pc_spectrum(pi_chord(c(60, 64, 67)),
                                  num_harmonics = 2,
                                  sigma = 100)
               %>% as.data.frame(),
               readRDS(system.file("stability-tests/smooth-pc-spectrum-60-64-67-v2.rds",
                                   package = "hrep")))
})

test_that("compare with Milne's original version", {
  cor(
    smooth_pc_spectrum("60 64 67"),
    milne_pc_spectrum("60 64 67")
  ) %>% expect_gt(0.998)

  cor(
    smooth_pc_spectrum("60 61 62"),
    milne_pc_spectrum("60 61 62")
  ) %>% expect_gt(0.997)

  cor(
    smooth_pc_spectrum("60 61 62", num_harmonics = 1),
    milne_pc_spectrum("60 61 62", num_harmonics = 1)
  ) %>% expect_equal(1)
})

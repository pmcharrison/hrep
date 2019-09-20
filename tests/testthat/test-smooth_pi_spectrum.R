test_that("sanity checks", {
  expect_is(smooth_pi_spectrum("0 4 7"),
            "smooth_pi_spectrum")

  expect_equal(smooth_pi_spectrum("0 4 7"),
               smooth_pi_spectrum(pi_chord(c(0, 4, 7))))


  expect_equal(smooth_pi_spectrum(pi_chord(c(0, 4, 7))),
               smooth_pi_spectrum(pi_chord(c(0, 4, 7)) %>% fr_chord()))

})

test_that("regression tests", {
  if (FALSE) { # Saving reference outputs
    smooth_pi_spectrum(pi_chord(c(60, 64, 67))) %>% as.data.frame() %>%
      saveRDS("inst/stability-tests/smooth-pi-spectrum-60-64-67.rds")

    smooth_pi_spectrum(pi_chord(c(60, 64, 67)), num_harmonics = 2, sigma = 100) %>%
      as.data.frame() %>%
      saveRDS("inst/stability-tests/smooth-pi-spectrum-60-64-67-v2.rds")
  }

  expect_equal(smooth_pi_spectrum(pi_chord(c(60, 64, 67))) %>% as.data.frame(),
               readRDS(system.file("stability-tests/smooth-pi-spectrum-60-64-67.rds",
                                   package = "hrep")))

  expect_equal(smooth_pi_spectrum(pi_chord(c(60, 64, 67)),
                                  num_harmonics = 2,
                                  sigma = 100)
               %>% as.data.frame(),
               readRDS(system.file("stability-tests/smooth-pi-spectrum-60-64-67-v2.rds",
                                   package = "hrep")))
})

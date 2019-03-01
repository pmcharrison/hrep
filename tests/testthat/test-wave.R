context("test-wave")

test_that("examples", {
  wave(.sparse_fr_spectrum(400, 1)) %>% as.numeric %>%
    expect_equal(
      seq(from = 0, to = 1, length.out = 44101) %>%
        {.[1:44100]} %>%
        {sin(2 * pi * 400 * .)}
    )

  sparse_fr_spectrum(fr_chord(400), num_harmonics = 1) %>%
    expect_equal(.sparse_fr_spectrum(400, 1),
                 tolerance = 1e-7)

  expect_equal(length(wave(60)), 44100)
  expect_equal(wave(60), wave(pi_chord(60)))

  expect_error(wave(milne_pc_spectrum(60)),
               "cannot translate smooth spectra to pi_chord representations")
})

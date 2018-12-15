context("test-is-equal-tempered")

library(magrittr)

test_that("examples", {
  c(0, 4, 7) %>% pi_chord %>% is.equal_tempered %>% expect_true
  c(0, 4.5, 7) %>% pi_chord %>% is.equal_tempered %>% expect_false

  c(0, 4, 7) %>% pc_chord %>% is.equal_tempered %>% expect_true
  c(0, 4.5, 7) %>% pc_chord %>% is.equal_tempered %>% expect_false

  c(0, 4, 7) %>% pc_set %>% is.equal_tempered %>% expect_true
  c(0, 4.5, 7) %>% pc_set %>% is.equal_tempered %>% expect_false

  c(0, 4, 7) %>% pi_chord %>% pi_sparse_spectrum %>% is.equal_tempered %>% expect_false
  c(0, 4, 7) %>% pi_chord %>% pi_sparse_spectrum() %>% is.equal_tempered %>% expect_false
})

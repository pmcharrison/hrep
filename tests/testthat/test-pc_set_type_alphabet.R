test_that("misc", {
  pc_set_type_alphabet$by_id %>% length() %>% expect_equal(351)
  pc_set_type_alphabet$by_chord %>% length() %>% expect_equal(351)

  pc_set_type_alphabet$by_id %>% anyDuplicated() %>% expect_equal(0)
  pc_set_type_alphabet$by_chord %>% names() %>% anyDuplicated() %>% expect_equal(0)
})

test_that("examples from paper", {
  c(0) %>% pc_set_type() %>% encode() %>% as.integer() %>% expect_equal(1)
  c(0, 1) %>% pc_set_type() %>% encode() %>% as.integer() %>% expect_equal(2)
  c(0, 2) %>% pc_set_type() %>% encode() %>% as.integer() %>% expect_equal(3)
  c(0, 1, 2) %>% pc_set_type() %>% encode() %>% as.integer() %>% expect_equal(4)
  c(0, 3) %>% pc_set_type() %>% encode() %>% as.integer() %>% expect_equal(5)
})

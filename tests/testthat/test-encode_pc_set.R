context("encode_pc_set")

test_that("encoding and decoding pc-sets", {
  rand <- sample(4e3, 20)
  rand %>% coded_vec("pc_set") %>% decode %>% encode %>% as.integer %>%
    expect_equal(rand)
})

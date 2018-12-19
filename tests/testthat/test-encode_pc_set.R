context("encode_pc_set")

test_that("encoding and decoding pc-sets", {
  expect_equal(encode(pc_set(0)), coded_vec(1, "pc_set"))
  rand <- sample(4e3, 20)
  rand %>% coded_vec("pc_set") %>% decode %>% encode %>% as.integer %>%
    expect_equal(rand)
})

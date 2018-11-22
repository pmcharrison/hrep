# context("get_chord_alphabet_from_corpus")
#
# corpus <- combine_corpora(
#   HarmonyCorpora::classical,
#   HarmonyCorpora::popular,
#   HarmonyCorpora::jazz
# )
#
# chord_alphabet <- get_chord_alphabet_from_corpus(corpus)
# pc_alphabet <- get_pc_set_alphabet_from_corpus(corpus)
#
# test_that("format", {
#   expect_is(
#     chord_alphabet,
#     "integer"
#     )
# })
#
# test_that("Approximate size of chord alphabets", {
#   expect_gt(length(chord_alphabet), 1000)
#   expect_lt(length(chord_alphabet), 1500)
# })
#
# test_that("Approximate size of pitch-class set alphabets", {
#   pc_alphabet <- get_pc_set_alphabet_from_corpus(corpus)
#   expect_gt(length(pc_alphabet), 400)
#   expect_lt(length(pc_alphabet), 600)
# })
#
# test_that("Format of alphabets", {
#   expect_true(is.numeric(chord_alphabet) || is.integer(chord_alphabet))
#
#   expect_true(all(sapply(pc_alphabet,
#                          function(x) is.numeric(x) || is.integer(x))))
#
#   chord_alphabet_decoded <- get_chord_alphabet_from_corpus(corpus, decode = TRUE)
#   expect_true(all(sapply(chord_alphabet_decoded, is.chord)))
# })

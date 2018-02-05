get_chord_storage_key <- function(chord) {
  assertthat::assert_that(
    is.numeric(chord)
  )
  paste(chord, collapse = " ")
}

#' @export
encode_chord <- function(chord) {
  HarmonyUtils::chord_alphabet$by_chord
  encode_chords(chords = list(chord))
}

#' @export
encode_chords <- function(chords) {
  assertthat::assert_that(
    is.list(chords)
  )
  keys <- lapply(chords, get_chord_storage_key)
  hash::values(HarmonyUtils::chord_alphabet$by_chord, keys) %>% unname
}

#' @export
decode_chord <- function(chord) {
  decode_chords(chord)[[1]]
}

#' @export
decode_chords <- function(chords) {
  assertthat::assert_that(
    is.numeric(chords)
  )
  HarmonyUtils::chord_alphabet$by_id[chords]
}

#' @export
get_chord_alphabet_from_corpus <- function(
  corpus, decode = FALSE
) {
  assertthat::assert_that(is(corpus, "Corpus"))
  corpus %>%
    (HarmonyCorpora::get_chord_counts) %>%
    names %>%
    as.integer %>%
    (function(x) if (decode) decode_chords(x) else x)
}

#' @export
get_chord_alphabet_size <- function() {
  length(chord_alphabet$by_id)
}

setGeneric("get_chord_storage_key",
           function(chord) standardGeneric("get_chord_storage_key"))
setMethod(
  "get_chord_storage_key", signature(chord = "numeric"),
  function(chord) {
    paste(chord, collapse = " ")
  }
)
setMethod(
  "get_chord_storage_key", signature(chord = "chord"),
  function(chord) {
    get_chord_storage_key(as.integer(chord))
  }
)

#' @export
is.chord <- function(x) is(x, "chord")

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
  lapply(HarmonyUtils::chord_alphabet$by_id[chords], as.chord)
}

#' @export
get_chord_alphabet_from_corpus <- function(
  corpus, decode = FALSE
) {
  assertthat::assert_that(is(corpus, "harmony_corpus"))
  corpus %>%
    get_chord_counts %>%
    names %>%
    as.integer %>%
    (function(x) if (decode) decode_chords(x) else x)
}

#' @export
get_chord_alphabet_size <- function() {
  length(HarmonyUtils::chord_alphabet$by_id)
}

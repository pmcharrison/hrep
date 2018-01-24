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
    is.list(chord_alphabet),
    is.numeric(chords)
  )
  HarmonyUtils::chord_alphabet$by_id[chords]
}

#' @export
get_chord_alphabet_from_dataset <- function(
  dataset, decode = FALSE
) {
  dataset %>%
    (function(x) do.call(c, x)) %>%
    unique %>%
    sort %>%
    (function(x) if (decode) decode_chords(x) else x)
}

#' @export
get_chord_alphabet_from_datasets <- function(
  datasets = list(
    HarmonyCorpora::classical,
    HarmonyCorpora::popular,
    HarmonyCorpora::jazz
  ),
  decode = FALSE
) {
  datasets %>%
    (function(x) do.call(c, x)) %>%
    get_chord_alphabet_from_dataset(decode = decode)
}

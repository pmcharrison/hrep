#' @export
get_pc_set_storage_key <- function(pc_set) {
  assertthat::assert_that(
    is.numeric(pc_set)
  )
  paste(pc_set, collapse = " ")
}

#' @export
get_pc_set_alphabet_from_corpus <- function(
  corpus, encode = FALSE
) {
  if (encode) {
    stop("Encoding not yet supported for pitch-class sets")
  }
  get_chord_alphabet_from_corpus(corpus) %>%
    decode_chords %>%
    lapply(HarmonyUtils::convert_pitch_to_pc_set) %>%
    unique %>%
    (function(x) x[order(vapply(x, function(y) {
      paste(y, collapse = " ")
    }, character(1)))])
}

#' @export
encode_pc_set <- function(pc_set) {
  encode_pc_sets(list(pc_set))
}

#' @export
encode_pc_sets <- function(pc_sets) {
  keys <- lapply(pc_sets, get_pc_set_storage_key)
  hash::values(HarmonyUtils::pc_set_alphabet$by_pc_set,
               keys) %>% unname
}

#' @export
decode_pc_sets <- function(pc_sets) {
  HarmonyUtils::pc_set_alphabet$by_id[pc_sets]
}

#' @param chord_id Vectorised
#' @export
map_chord_id_to_pc_set_id <- function(chord_id) {
  HarmonyUtils::chord_id_to_pc_set_id_map[chord_id]
}

#' @include classes.R
#' @include generics.R

setMethod(
  "get_pc_set_storage_key", signature(pc_set = "pc_set"),
  function(pc_set) get_pc_set_storage_key(as.numeric(pc_set))
)
setMethod(
  "get_pc_set_storage_key", signature(pc_set = "numeric"),
  function(pc_set) paste(pc_set, collapse = " ")
)

#' @export
get_pc_set_alphabet_from_corpus <- function(
  corpus, encode = FALSE
) {
  if (encode) {
    stop("Encoding not yet supported for pitch-class sets")
  }
  get_chord_alphabet_from_corpus(corpus) %>%
    decode_chords %>%
    lapply(as.integer) %>%
    lapply(convert_pitch_to_pc_set) %>%
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
  HarmonyUtils::pc_set_alphabet$by_id[pc_sets] %>%
    lapply(as.pc_set)
}

#' @param chord_id Vectorised
#' @export
map_chord_id_to_pc_set_id <- function(chord_id) {
  assertthat::assert_that(all(na.omit(chord_id) == round(na.omit(chord_id))))
  HarmonyUtils::chord_id_to_pc_set_id_map[as.integer(chord_id)]
}

get_pc_set_storage_key <- function(pc_set) {
  if (!is(pc_set, "pc_set")) stop()
  paste(as.integer(pc_set), collapse = " ")
}

#' @export
get_pc_set_alphabet_from_corpus <- function(
  corpus, encode = FALSE
) {
  if (encode) {
    stop("Encoding not yet supported for pitch-class sets")
  }
  get_pc_chord_alphabet_from_corpus(corpus) %>%
    decode_pc_chords %>%
    lapply(as.integer) %>%
    lapply(pitch_to_pc_set) %>%
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
  unname(hash::values(pc_set_alphabet$by_pc_set, keys))
}

#' @export
decode_pc_set <- function(pc_set) {
  decode_pc_sets(pc_set)[[1]]
}

#' @export
decode_pc_sets <- function(pc_sets) {
  max_id <- length(pc_set_alphabet$by_id)
  if (!is.numeric(pc_sets) ||
      any(is.na(pc_sets) |
          pc_sets < 1 |
          pc_sets > max_id |
          round(pc_sets) != pc_sets)) {
    stop("All pc_set ids must be integers between 1 and ", max_id, ".")
  }
  lapply(pc_set_alphabet$by_id[pc_sets],
         function(x) pc_set(x, safe = FALSE))
}

#' @param pc_chord_id Vectorised
#' @export
map_pc_chord_id_to_pc_set_id <- function(pc_chord_id) {
  pc_chord_id_to_pc_set_id_map[as.integer(pc_chord_id)]
}

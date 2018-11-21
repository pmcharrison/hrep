get_pc_set_storage_key <- function(pc_set) {
  if (!is(pc_set, "pc_set")) stop()
  x <- as.numeric(pc_set)
  if (!checkmate::qtest(x, "X"))
    stop("cannot encode non-integer pitch-class set")
  paste(x, collapse = " ")
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
    lapply(pi_to_pc_set) %>%
    unique %>%
    (function(x) x[order(vapply(x, function(y) {
      paste(y, collapse = " ")
    }, character(1)))])
}

#' @export
encode.pc_set <- function(pc_set) {
  key <- get_pc_set_storage_key(pc_set)
  pc_set_alphabet$by_pc_set[[key]]
}

decode_pc_set <- function(x) {
  max_id <- length(pc_set_alphabet$by_id)
  if (!is.numeric(x) ||
      any(is.na(x) |
          x < 1 |
          x > max_id |
          round(x) != x)) {
    stop("All pc_set ids must be integers between 1 and ", max_id, ".")
  }
  lapply(pc_set_alphabet$by_id[x], function(x) pc_set(x))
}

#' @param pc_chord_id Vectorised
#' @export
map_pc_chord_id_to_pc_set_id <- function(pc_chord_id) {
  pc_chord_id_to_pc_set_id_map[as.integer(pc_chord_id)]
}

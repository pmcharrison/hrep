get_pc_chord_storage_key <- function(pc_chord) {
  if (!is(pc_chord, "pc_chord")) stop()
  paste(as.integer(pc_chord), collapse = " ")
}

#' @export
is.pc_chord <- function(x) is(x, "pc_chord")

#' @export
encode.pc_chord <- function(x) {
  key <- get_pc_chord_storage_key(pc_chord)
  pc_chord_alphabet$by_pc_chord[[key]]
}

decode_pc_chord <- function(x) {
  checkmate::qassert(x, "X")
  pc_chord_alphabet$by_id[x]
}

#' @export
get_pc_chord_alphabet_from_corpus <- function(
  corpus, decode = FALSE
) {
  stop("needs refactoring")
  stopifnot(is(corpus, "harmony_corpus"))
  corpus %>%
    get_chord_counts %>%
    names %>%
    as.integer %>%
    (function(x) if (decode) decode_pc_chords(x) else x)
}

#' @export
get_pc_chord_alphabet_size <- function() {
  length(pc_chord_alphabet$by_id)
}

# The output of this function is cached in data/ and can be accessed
# when the hutil pakage is loaded.
get_pc_chord_alphabet <- function() {
  pc_chord_alphabet <- unlist(lapply(0:11, list_pc_chords_with_bass_note),
                           recursive = FALSE)
  pc_chord_ids <- seq_along(pc_chord_alphabet)
  map <- new.env(parent = emptyenv())
  for (pc_chord_id in pc_chord_ids) {
    pc_chord <- pc_chord_alphabet[[pc_chord_id]]
    key <- get_pc_chord_storage_key(pc_chord)
    map[[key]] <- pc_chord_id
  }
  list(by_id = pc_chord_alphabet,
       by_pc_chord = map)
}

list_pc_chords_with_bass_note <- function(bass_pc) {
  sets::set_power(x = setdiff(0:11, bass_pc)) %>%
    as.list %>%
    lapply(function(y) pc_chord(bass_pc, as.integer(y)))
}

get_chord_storage_key <- function(chord) {
  if (!is(chord, "chord")) stop()
  paste(as.integer(chord), collapse = " ")
}

#' @export
is.chord <- function(x) is(x, "chord")

#' @export
encode_chord <- function(chord) {
  key <- get_chord_storage_key(chord)
  hutil::chord_alphabet$by_chord[[key]]
}

#' @export
encode_chords <- function(chords) {
  stopifnot(is.list(chords))
  vapply(chords, encode_chord, integer(1))
}

#' @export
decode_chord <- function(chord) {
  hutil::chord_alphabet$by_id[[chords]]
}

#' @export
decode_chords <- function(chords) {
  stopifnot(is.numeric(chords))
  hutil::chord_alphabet$by_id[chords]
}

#' @export
get_chord_alphabet_from_corpus <- function(
  corpus, decode = FALSE
) {
  stopifnot(is(corpus, "harmony_corpus"))
  corpus %>%
    get_chord_counts %>%
    names %>%
    as.integer %>%
    (function(x) if (decode) decode_chords(x) else x)
}

#' @export
get_chord_alphabet_size <- function() {
  length(hutil::chord_alphabet$by_id)
}

# The output of this function is cached in data/ and can be accessed
# when the hutil pakage is loaded.
get_chord_alphabet <- function() {
  chord_alphabet <- unlist(lapply(0:11, list_chords_with_bass_note),
                           recursive = FALSE)
  chord_ids <- seq_along(chord_alphabet)
  map <- new.env(parent = emptyenv())
  for (chord_id in chord_ids) {
    chord <- chord_alphabet[[chord_id]]
    key <- get_chord_storage_key(chord)
    map[[key]] <- chord_id
  }
  list(by_id = chord_alphabet,
       by_chord = map)
}

list_chords_with_bass_note <- function(bass_pc) {
  sets::set_power(x = setdiff(0:11, bass_pc)) %>%
    as.list %>%
    lapply(function(y) pc_chord(bass_pc, as.integer(y)))
}

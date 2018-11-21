#' @param bass_pc Numeric scalar corresponding to bass pitch class
#' @param pc_set Numeric vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
pc_chord <- function(bass_pc, other_pc) {
  checkmate::qassert(bass_pc, "N1[0,12)")
  checkmate::qassert(other_pc, "N[0,12)")
  other_pc <- setdiff(sort(unique(other_pc)), bass_pc)
  x <- c(bass_pc, other_pc)
  class(x) <- "pc_chord"
  x
}

#' @export
print.pc_chord <- function(x, ...) {
  cat("Pitch-class chord: ",
      "[", get_bass_pc(x), "] ",
      paste(get_non_bass_pc(x), collapse = " "), "\n",
      sep = "")
}

#' @export
as.pc_chord <- function(x) UseMethod("as.pc_chord")
#' @export
as.pc_chord.numeric <- function(x) {
  pc_chord(bass_pc = x[1], other_pc = x[-1])
}
#' @export
as.pc_chord.pc_chord <- function(x) {
  x
}

#' @export
get_bass_pc <- function(x) UseMethod("get_bass_pc")
#' @export
get_bass_pc.pc_chord <- function(x) x[1]

#' @export
get_non_bass_pc <- function(x) UseMethod("get_non_bass_pc")
#' @export
get_non_bass_pc.pc_chord <- function(x) pc_set(x[- 1])

#' @export
as.numeric.pc_chord <- function(x) {
  class(x) <- "numeric"
  x
}

#' @export
as.integer.pc_chord <- function(x) {
  as.integer(as.numeric(x))
}

#' @export
as.pc_set.pc_chord <- function(x) {
  pc_set(sort(unique(as.numeric(x))))
}

#' @export
normalise_bass.pc_chord <- function(x) {
  transpose(x, - get_bass_pc(x))
}

#' @export
get_transpositions.pc_chord <- function(x) {
  ref <- normalise_bass(x)
  lapply(0:11, function(int) transpose(ref, int))
}

get_pc_chord_storage_key <- function(pc_chord) {
  if (!is(pc_chord, "pc_chord")) stop()
  x <- as.numeric(pc_chord)
  if (!checkmate::qtest(x, "X"))
    stop("cannot encode non-integer pitch-class chord")
  paste(x, collapse = " ")
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


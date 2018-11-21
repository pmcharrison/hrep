#' @param bass_pc Integer scalar corresponding to bass pitch class
#' @param pc_set Integer vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
pc_chord <- function(bass_pc, non_bass_pc_set, safe = TRUE) {
  if (safe) {
    if (!is.numeric(non_bass_pc_set)) stop("non_bass_pc_set must be numeric")
    if (!all(non_bass_pc_set == round(non_bass_pc_set))) stop(
      "non_bass_pc_set must be all whole numbers")
    if (!all(non_bass_pc_set >= 0 & non_bass_pc_set < 12)) stop(
      "non_bass_pc_set must be within 0 and 12")
    if (anyDuplicated(non_bass_pc_set)) stop(
      "No duplicates allowed in non_bass_pc_set")
    if (!is.numeric(bass_pc)) stop("bass_pc must be numeric")
    if (!bass_pc == round(bass_pc)) stop("bass_pc must be a whole number")
    if (!bass_pc >= 0) stop("bass_pc must be 0 or greater")
    if (!bass_pc < 12) stop("bass_pc must be smaller than 12")
    if (!length(bass_pc) == 1) stop("bass_pc must be length 1")
    if (bass_pc %in% non_bass_pc_set) stop(
      "bass_pc cannot be contained in non_bass_pc_set")
    bass_pc <- as.integer(bass_pc)
    non_bass_pc_set <- sort(non_bass_pc_set)
  }
  x <- c(bass_pc, non_bass_pc_set)
  class(x) <- "chord"
  x
}

#' @export
print.chord <- function(x, ...) {
  cat("Chord: ",
      "[", get_bass_pc(x), "] ",
      paste(get_non_bass_pc_set(x), collapse = " "), "\n",
      sep = "")
}

#' @export
as.chord <- function(x, safe = TRUE) UseMethod("as.chord")
#' @export
as.chord.numeric <- function(x, safe = TRUE) {
  if (safe) {
    pc_chord(bass_pc = x[1], non_bass_pc_set = x[-1], safe = safe)
  } else {
    class(x) <- "chord"
    x
  }
}
#' @export
as.chord.chord <- function(x, safe = TRUE) {
  if (safe) {
    pc_chord(bass_pc = get_bass_pc(x),
             non_bass_pc_set = get_non_bass_pc_set(x),
             safe = TRUE)
  } else x
}

#' @export
get_bass_pc <- function(x) UseMethod("get_bass_pc")
#' @export
get_bass_pc.chord <- function(x) x[1]

#' @export
get_non_bass_pc_set <- function(x, safe = TRUE) UseMethod("get_non_bass_pc_set")
#' @export
get_non_bass_pc_set.chord <- function(x, safe = TRUE) new_pc_set(x[- 1],
                                                                 safe = safe)

#' @export
get_pc_set <- function(x, safe = TRUE) UseMethod("get_pc_set")
#' @export
get_pc_set.chord <- function(x, safe = TRUE) new_pc_set(sort(as.integer(x)),
                                                        safe = safe)

#' @export
normalise_bass.chord <- function(x) {
  transpose(x, - get_bass_pc(x))
}

#' @export
get_transpositions.chord <- function(x) {
  ref <- normalise_bass(x)
  lapply(0:11, function(int) transpose(ref, int, safe = FALSE))
}

# Not sure if the following are needed at the moment

# setMethod(
#   "normalise_bass", signature(x = "harmony_composition"),
#   function(x) {
#     x %>% as.integer %>% decode_chords %>% lapply(normalise_bass) %>%
#       as.harmony_composition
#   }
# )
# setMethod(
#   "normalise_bass", signature(x = "harmony_corpus"),
#   function(x) {
#     x@compositions <- lapply(x@compositions, normalise_bass)
#     x
#   }
# )

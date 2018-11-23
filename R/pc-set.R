#' @export
pc_set <- function(pitch_classes) {
  checkmate::qassert(pitch_classes, "X[0,12)")
  pitch_classes <- sort(unique(pitch_classes))
  class(pitch_classes) <- "pc_set"
  pitch_classes
}

#' @export
as.pc_set <- function(x) UseMethod("as.pc_set")
#' @export
as.pc_set.pc_set <- function(x) x
#' @export
as.pc_set.numeric <- function(x, ...) {
  pc_set(x)
}

#' @export
print.pc_set <- function(x, ...) {
  cat("Pitch-class set: ", as.character(x), "\n", sep = "")
}

#' @export
as.character.pc_set <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

#' @export
encode.pc_set <- function(x, ...) {
  checkmate::qassert(x, "X")
  key <- as.character(x)
  pc_set_alphabet$by_pc_set[[key]]
}

# Vectorised
decode.coded_vec_pc_set <- function(x) {
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

#' #' @export
#' get_pc_set_alphabet_from_corpus <- function(
#'   corpus, encode = FALSE
#' ) {
#'   if (encode) {
#'     stop("Encoding not yet supported for pitch-class sets")
#'   }
#'   get_pc_chord_alphabet_from_corpus(corpus) %>%
#'     decode_pc_chords %>%
#'     lapply(as.integer) %>%
#'     lapply(pi_to_pc_set) %>%
#'     unique %>%
#'     (function(x) x[order(vapply(x, function(y) {
#'       paste(y, collapse = " ")
#'     }, character(1)))])
#' }

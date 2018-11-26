#' @export
.pc_set <- function(...) {
  pc <- unclass(c(...))
  checkmate::qassert(pc, "N[0,12)")
  stopifnot(!anyDuplicated(pc), isTRUE(all.equal(pc, sort(pc))))
  class(pc) <- "pc_set"
  pc
}

#' @export
pc_set <- function(x) {
  UseMethod("pc_set")
}

#' @export
pc_set.numeric <- function(x) {
  pc_set(pi_chord(unclass(x)))
}

#' @export
pc_set.pc_set <- function(x) {
  x
}

#' @export
pc_set.pc_chord <- function(x) {
  .pc_set(sort(as.numeric(x)))
}

#' @export
pc_set.pi_chord <- function(x) {
  x <- as.numeric(x)
  .pc_set(sort(unique(pi_to_pc(x))))
}

#' @export
pc_set.pc_set_norm_order <- function(x) {
  pc_set(sort(x))
}

#' @export
pc_set.pc_set_norm_form <- function(x) {
  .pc_set(as.numeric(x))
}

#' @export
is.pc_set <- function(x) is(x, "pc_set")

#' @export
print.pc_set <- function(x, ...) {
  cat("Pitch-class set: ", as.character(x), "\n", sep = "")
}

#' @export
view.pc_set <- function(x, ...) {
  view(pi_chord(x), ...)
}

#' @export
as.character.pc_set <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

#' @export
c.pc_set <- function(...) {
  x <- lapply(list(...), unclass)
  x <- do.call(c, x)
  pc_set(sort(unique(x)))
}

#' @export
encode.pc_set <- function(x, ...) {
  checkmate::qassert(x, "X")
  key <- as.character(x)
  hrep::pc_set_alphabet$by_pc_set[[key]]
}

# Vectorised
decode.coded_vec_pc_set <- function(x) {
  max_id <- length(hrep::pc_set_alphabet$by_id)
  if (!is.numeric(x) ||
      any(is.na(x) |
          x < 1 |
          x > max_id |
          round(x) != x)) {
    stop("All pc_set ids must be integers between 1 and ", max_id, ".")
  }
  lapply(hrep::pc_set_alphabet$by_id[x], function(x) pc_set(x))
}

#' @param pc_chord_id Vectorised
#' @export
map_pc_chord_id_to_pc_set_id <- function(pc_chord_id) {
  hrep::pc_chord_id_to_pc_set_id_map[as.integer(pc_chord_id)]
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

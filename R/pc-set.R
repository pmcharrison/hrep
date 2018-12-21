.pc_set <- function(...) {
  pc <- unclass(c(...))
  checkmate::qassert(pc, "N+[0,12)")
  stopifnot(!anyDuplicated(pc), isTRUE(all.equal(pc, sort(pc))))
  class(pc) <- "pc_set"
  pc
}

#' Pitch-class set
#'
#' This function represents an object as a pitch-class set.
#' @param x Object to represent as a pitch-class set.
#' @return Returns an object of class \code{pc_set}.
#' @export
#' @rdname pc_set
pc_set <- function(x) {
  UseMethod("pc_set")
}

#' @export
#' @rdname pc_set
pc_set.numeric <- function(x) {
  pc_set(pi_chord(x))
}

#' @export
#' @rdname pc_set
pc_set.pc_set <- function(x) {
  x
}

#' @export
#' @rdname pc_set
pc_set.pc_chord <- function(x) {
  .pc_set(sort(as.numeric(x)))
}

#' @export
#' @rdname pc_set
pc_set.pi_chord <- function(x) {
  x <- as.numeric(x)
  .pc_set(sort(unique(pi_to_pc(x))))
}

#' @export
#' @rdname pc_set
pc_set.fr_chord <- function(x) {
  pc_set(pi_chord(x))
}

#' @rdname pc_set
#' @export
pc_set.pc_set_norm_order <- function(x) {
  pc_set(sort(x))
}

#' @rdname pc_set
#' @export
pc_set.pc_set_norm_form <- function(x) {
  .pc_set(as.numeric(x))
}

#' Check for type "pc_set"
#'
#' Checks whether an object is of type "pc_set".
#' @param x Object to test.
#' @return Scalar logical.
#' @export
is.pc_set <- function(x) is(x, "pc_set")

#' @export
print.pc_set <- function(x, ...) {
  cat("Pitch-class set: ", as.character(x), "\n", sep = "")
}

#' @rdname view
#' @export
view.pc_set <- function(x, ...) {
  view(pi_chord(x), ...)
}

#' @rdname pc_set
#' @export
pc_set.character <- function(x) {
  stopifnot(length(x) == 1L)
  y <- as.numeric(strsplit(x, split = " ")[[1]])
  if (anyNA(y)) stop("malformed character input, should be of the form ",
                     "'0 4 7'")
  pc_set(y)
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

#' @rdname encode
#' @export
encode.pc_set <- function(x) {
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

#' Map pitch-class chords to pitch-class sets
#'
#' This vectorised function maps encoded pitch-class chords ("pc_chord")
#' (as produced by \code{\link{encode}})
#' to encoded pitch-class sets ("pc_set").
#' @param pc_chord_id Numeric vector of pitch-class chord codes.
#' @return Numeric vector of pitch-class set codes.
#' @export
map_pc_chord_id_to_pc_set_id <- function(pc_chord_id) {
  hrep::pc_chord_id_to_pc_set_id_map[as.integer(pc_chord_id)]
}

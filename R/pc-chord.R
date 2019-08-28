#' Pitch-class chord constructor
#'
#' This hidden function constructs a pitch-class chord object.
#' It is unforgiving with respect to input formats,
#' unlike \code{\link{pc_chord}}.
#' @param bass_pc Numeric scalar corresponding to bass pitch class
#' @param other_pc Numeric vector corresponding to pitch-class set,
#' may optionally include the bass pitch class
#' @keywords internal
.pc_chord <- function(bass_pc, other_pc = numeric()) {
  bass_pc <- as.numeric(bass_pc)
  other_pc <- as.numeric(other_pc)
  checkmate::qassert(bass_pc, "N1[0,12)")
  checkmate::qassert(other_pc, "N[0,12)")
  stopifnot(!anyDuplicated(other_pc),
            !bass_pc %in% other_pc,
            isTRUE(all.equal(other_pc, sort(other_pc))))
  x <- c(bass_pc, other_pc)
  class(x) <- c("pc_chord", "chord", class(x))
  x
}

#' Pitch-class chord
#'
#' This function represents an object as a pitch-class chord.
#' A pitch-class chord is defined by the combination of
#' a pitch-class set and a bass pitch class.
#' @param x Object to represent as a pitch-class chord.
#' @return Returns an object of class \code{pc_chord}.
#' @export
#' @rdname pc_chord
pc_chord <- function(x) {
  UseMethod("pc_chord")
}

#' @export
#' @rdname pc_chord
pc_chord.numeric <- function(x) {
  bass_pc <- pi_to_pc(x[1])
  other_pc <- setdiff(sort(unique(pi_to_pc(x[-1]))),
                      bass_pc)
  .pc_chord(bass_pc, other_pc)
}

#' @export
pc_chord.chord <- function(x) {
  stop("cannot translate this object to pc_chord format")
}

#' @export
#' @rdname pc_chord
pc_chord.pc_set <- function(x) {
  x <- as.numeric(x)
  .pc_chord(x[1], x[-1])
}

#' @export
#' @rdname pc_chord
pc_chord.pc_chord <- function(x) {
  x
}

#' @export
#' @rdname pc_chord
pc_chord.pc_chord_type <- function(x) {
  .pc_chord(bass_pc = x[1], other_pc = x[-1])
}

#' @export
#' @rdname pc_chord
pc_chord.pi_chord <- function(x) {
  pc_chord(as.numeric(x))
}

#' @export
#' @rdname pc_chord
pc_chord.fr_chord <- function(x) {
  pc_chord(pi_chord(x))
}

#' @export
as.numeric.pc_chord <- function(x, ...) {
  class(x) <- "numeric"
  x
}

#' @export
as.integer.pc_chord <- function(x, ...) {
  as.integer(as.numeric(x))
}

#' @export
print.pc_chord <- function(x, ...) {
  cat("Pitch-class chord: ",
      "[", get_bass_pc(x), "] ",
      paste(get_non_bass_pc(x), collapse = " "), "\n",
      sep = "")
}

#' @rdname view
#' @export
view.pc_chord <- function(x, ...) {
  view(.pi_chord(c(48 + get_bass_pc(x),
                   60 + get_non_bass_pc(x))),
       ...)
}

#' @rdname pc_chord
#' @export
pc_chord <- function(x) UseMethod("pc_chord")

#' Get bass pitch class
#'
#' Gets the bass pitch class of a sonority.
#' @param x Object to analyse.
#' @return The bass pitch class, as a numeric scalar.
#' @rdname get_bass_pc
#' @export
get_bass_pc <- function(x) UseMethod("get_bass_pc")

#' @rdname get_bass_pc
#' @export
get_bass_pc.default <- function(x) {
  get_bass_pc(pc_chord(x))
}

#' @rdname get_bass_pc
#' @export
get_bass_pc.pc_chord <- function(x) x[1]

#' Get non-bass pitch classes
#'
#' Gets the non-bass pitch classes in a sonority.
#' @param x Object to analyse.
#' @return The non-bass pitch classes, as a numeric vector.
#' @rdname get_non_bass_pc
#' @export
get_non_bass_pc <- function(x) {
  # Note: we don't return a pitch-class set, because pitch class sets
  # are not allowed to be empty
  UseMethod("get_non_bass_pc")
}

#' @rdname get_bass_pc
#' @export
get_non_bass_pc.default <- function(x) {
  get_non_bass_pc(pc_chord(x))
}

#' @rdname get_bass_pc
#' @export
get_non_bass_pc.pc_chord <- function(x) {
  x[- 1]
}

#' @rdname pc_chord
#' @export
pc_chord.character <- function(x) {
  stopifnot(length(x) == 1L)
  y <- as.numeric(strsplit(x, split = " ")[[1]])
  if (anyNA(y)) stop("malformed character input, should be of the form ",
                     "'4 0 7'")
  pc_chord(y)
}

#' @export
as.character.pc_chord <- function(x, ...) {
  paste(as.numeric(x), collapse = " ")
}

#' Check for class "pc_chord"
#'
#' Checks whether an object is of class "pc_chord".
#' @param x Object to analyse
#' @return Logical scalar.
#' @export
is.pc_chord <- function(x) {
  is(x, "pc_chord")
}

#' @rdname encode
#' @export
encode.pc_chord <- function(x) {
  checkmate::qassert(x, "X")
  key <- as.character(x)
  hrep::pc_chord_alphabet$by_pc_chord[[key]]
}

decode.coded_vec_pc_chord <- function(x) {
  checkmate::qassert(x, "X")
  hrep::pc_chord_alphabet$by_id[x]
}

#' Edit bass pitch class
#'
#' Edits the bass pitch class of a sonority.
#' Throws an error if the proposed pitch class was not already
#' in the sonority's pitch-class set.
#'
#' @param x Original sonority.
#'
#' @param new
#' (Numeric scalar)
#' New bass pitch class.
#'
#' @return A sonority with the updated bass pitch class.
#'
#' @rdname edit_bass_pc
#' @export
edit_bass_pc <- function(x, new) {
  UseMethod("edit_bass_pc")
}

#' @rdname edit_bass_pc
#' @export
edit_bass_pc.pc_chord <- function(x, new) {
  checkmate::qassert(new, "N1")
  if (!new %in% x)
    stop("requested bass pitch class was not found in original chord")
  non_bass <- sort(setdiff(x, new))
  .pc_chord(new, non_bass)
}

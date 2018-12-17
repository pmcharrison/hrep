#' @param bass_pc Numeric scalar corresponding to bass pitch class
#' @param other_pc Numeric vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
.pc_chord <- function(bass_pc, other_pc = numeric()) {
  checkmate::qassert(bass_pc, "N1[0,12)")
  checkmate::qassert(other_pc, "N[0,12)")
  stopifnot(!anyDuplicated(other_pc),
            !bass_pc %in% other_pc,
            isTRUE(all.equal(other_pc, sort(other_pc))))
  # other_pc <- setdiff(sort(unique(other_pc)), bass_pc)
  x <- c(bass_pc, other_pc)
  class(x) <- "pc_chord"
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
  other_pc <- sort(unique(pi_to_pc(x[-1])))
  .pc_chord(bass_pc, other_pc)
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
pc_chord.pi_chord <- function(x) {
  x <- as.numeric(x)
  .pc_chord(bass_pc = pi_to_pc(x[1]), other_pc = pi_to_pc(x[-1]))
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
is.pc_chord <- function(x) is(x, "pc_chord")

#' @export
c.pc_chord <- function(...) {
  x <- lapply(list(...), unclass)
  x <- do.call(c, x)
}

#' @export
print.pc_chord <- function(x, ...) {
  cat("Pitch-class chord: ",
      "[", get_bass_pc(x), "] ",
      paste(get_non_bass_pc(x), collapse = " "), "\n",
      sep = "")
}

#' @export
view.pc_chord <- function(x, ...) {
  view(pi_chord(x), ...)
}

#' @export
pc_chord <- function(x) UseMethod("pc_chord")

#' @export
get_bass_pc <- function(x) UseMethod("get_bass_pc")
#' @export
get_bass_pc.pc_chord <- function(x) x[1]

#' @export
get_non_bass_pc <- function(x) UseMethod("get_non_bass_pc")
#' @export
get_non_bass_pc.pc_chord <- function(x) pc_set(x[- 1])

#' @export
get_transpositions.pc_chord <- function(x) {
  ref <- normalise_bass(x)
  lapply(0:11, function(int) tp(ref, int))
}

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

#' @export
is.pc_chord <- function(x) is(x, "pc_chord")

#' @export
encode.pc_chord <- function(x, ...) {
  checkmate::qassert(x, "X")
  key <- as.character(x)
  hrep::pc_chord_alphabet$by_pc_chord[[key]]
}

decode.coded_vec_pc_chord <- function(x) {
  checkmate::qassert(x, "X")
  hrep::pc_chord_alphabet$by_id[x]
}

#' @export
pc_chord_alphabet_size <- function() {
  length(hrep::pc_chord_alphabet$by_id)
}

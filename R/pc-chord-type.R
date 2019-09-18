#' Pitch-class chord type constructor
#'
#' This is the low-level construct for pitch-class chord type.
#' @keywords internal
.pc_chord_type <- function(x) {
  checkmate::qassert(x, "N+[0,12)")
  x <- as.numeric(x)
  stopifnot(x[1] == 0,
            !anyDuplicated(x),
            isTRUE(all.equal(x, sort(x))))
  class(x) <- c("pc_chord_type", "pc_chord", "chord")
  x
}

#' Pitch-class chord type
#'
#' This function represents an object as a pitch-class chord type.
#' A pitch-class chord type is defined as the chord's pitch class set
#' expressed relative to the pitch class of the bass note.
#' @param x Object to represent as a pitch-class chord type.
#' @return Returns an object of class \code{pc_chord_type}.
#' @rdname pc_chord_type
#' @export
pc_chord_type <- function(x) {
  UseMethod("pc_chord_type")
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.default <- function(x) {
  pc_chord_type(pi_chord(x))
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.pc_chord <- function(x) {
  .pc_chord_type(as.numeric(tp(x, - get_bass_pc(x))))
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.pi_chord <- function(x) {
  pc_chord_type(pc_chord(x))
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.pc_set <- function(x) {
  pc_chord_type(pc_set_type(x))
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.pc_set_type <- function(x) {
  .pc_chord_type(x)
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.pc_set_norm_order <- function(x) {
  pc_chord_type(pc_set_type(x))
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.smooth_spectrum <- function(x) {
  stop("no methods exist for coercing smooth spectra to pitch-class chord types")
}

#' @export
#' @rdname pc_chord_type
pc_chord_type.sparse_spectrum <- function(x) {
  stop("no methods exist for coercing sparse spectra to pitch-class chord types")
}

#' @rdname encode
#' @export
encode.pc_chord_type <- function(x) {
  checkmate::qassert(x, "X")
  key <- as.character(x)
  i <- as.integer(hrep::pc_chord_alphabet$by_pc_chord[[key]])
  coded_vec(i, "pc_chord_type")
}

decode.coded_vec_pc_chord_type <- function(x) {
  checkmate::qassert(x, "X")
  purrr::map(hrep::pc_chord_alphabet$by_id[x],
             ~ .pc_chord_type(as.integer(.)))
}

#' @export
print.pc_chord_type <- function(x, ...) {
  cat("Pitch-class chord type: ",
      as.character(x),
      sep = "")
}

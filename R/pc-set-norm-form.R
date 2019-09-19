.pc_set_type <- function(x, transposition = NULL) {
  checkmate::qassert(x, "N[0,12)")
  stopifnot(is.null(transposition) || checkmate::qtest(transposition, "N1"))
  stopifnot(!anyDuplicated(x))
  stopifnot(identical(x, sort(x)))
  class(x) <- c("pc_set_type", "pc_set", "chord")
  attr(x, "transposition") <- transposition
  x
}

#' Pitch-class set type
#'
#' Expresses a sonority as a pitch-class set type.
#' Pitch-class set types form equivalence classes of
#' pitch-class sets under transposition.
#' @param x Input sonority.
#' @return An object of class "pc_set_type".
#'
#' @note
#' When created from a pitch-class set, a pitch-class set type
#' is associated with the interval by which it was transposed
#' from the original pitch-class set.
#' This transposition can be accessed with the function \code{transposition()}.
#' @rdname pc_set_type
#' @export
pc_set_type <- function(x) {
  UseMethod("pc_set_type")
}

#' @rdname pc_set_type
#' @export
pc_set_type.default <- function(x) {
  pc_set_type(pc_set(x))
}

#' @rdname pc_set_type
#' @export
pc_set_type.pc_set <- function(x) {
  pc_set_type(pc_set_norm_order(x))
}

#' @rdname pc_set_type
#' @export
pc_set_type.pc_set_norm_order <- function(x) {
  x_num <- as.numeric(x)
  transposition <- x_num[1]
  res <- if (length(x_num) == 0) x_num else {
    (x_num - transposition) %% 12L
  }
  .pc_set_type(res, - transposition)
}

#' @rdname pc_set_type
#' @export
pc_set_type.pc_set_type <- function(x) x

#' @export
print.pc_set_type <- function(x, ...) {
  trans <- transposition(x)
  cat("Pitch-class set type: ",
      paste0("[", paste(as.numeric(x), collapse = ", "), "]\n"),
      sep = "")
  cat("Transposition from original pitch-class set: ",
      if (is.null(trans)) "unavailable" else trans, "\n",
      sep = "")
}

#' @rdname pc_set_type
#' @export
transposition.pc_set_type <- function(x) attr(x, "transposition")

#' @rdname encode
#' @export
encode.pc_set_type <- function(x) {
  encode(pc_set(x))
}

decode.coded_vec_pc_set_type <- function(x) {
  x <- coded_vec(x, "pc_set")
  lapply(decode(x), pc_set_type)
}

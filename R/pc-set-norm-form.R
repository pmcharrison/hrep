.pc_set_norm_form <- function(x, transposition = NULL) {
  checkmate::qassert(x, "N[0,12)")
  stopifnot(is.null(transposition) || checkmate::qtest(transposition, "N1"))
  stopifnot(!anyDuplicated(x))
  stopifnot(identical(x, sort(x)))
  class(x) <- c("pc_set_norm_form", "pc_set")
  attr(x, "transposition") <- transposition
  x
}

#' Pitch-class set (normal form)
#'
#' Expresses a sonority as a normal-form pitch-class set.
#' Normal-form pitch-class sets form equivalence classes of
#' pitch-class sets under transposition.
#' @param x Input sonority.
#' @return An object of class "pc_set_norm_form".
#'
#' @note
#' When created from a pitch-class set, a normal-form pitch-class set
#' is associated with the interval by which it was transposed
#' from the original pitch-class set.
#' This transposition can be accessed with the function
#' \code{\link{transposition}}.
#' @rdname pc_set_norm_form
#' @export
pc_set_norm_form <- function(x) {
  UseMethod("pc_set_norm_form")
}

#' @rdname pc_set_norm_form
#' @export
pc_set_norm_form.default <- function(x) {
  pc_set_norm_form(pc_set(x))
}

#' @rdname pc_set_norm_form
#' @export
pc_set_norm_form.pc_set <- function(x) {
  pc_set_norm_form(pc_set_norm_order(x))
}

#' @rdname pc_set_norm_form
#' @export
pc_set_norm_form.pc_set_norm_order <- function(x) {
  x_num <- as.numeric(x)
  transposition <- x_num[1]
  res <- if (length(x_num) == 0) x_num else {
    (x_num - transposition) %% 12L
  }
  .pc_set_norm_form(res, - transposition)
}

#' @rdname pc_set_norm_form
#' @export
pc_set_norm_form.pc_set_norm_form <- function(x) x

#' @export
print.pc_set_norm_form <- function(x, ...) {
  trans <- transposition(x)
  cat("Pitch-class set (normal form): ",
      paste0("[", paste(as.numeric(x), collapse = ", "), "]\n"),
      sep = "")
  cat("Transposition from original pitch-class set: ",
      if (is.null(trans)) "unavailable" else trans, "\n",
      sep = "")
}

#' @export
c.pc_set_norm_form <- function(...) {
  x <- lapply(list(...), pc_set)
  x <- do.call(c, x)
}

#' @rdname pc_set_norm_form
#' @export
transposition.pc_set_norm_form <- function(x) attr(x, "transposition")

#' @rdname encode
#' @export
encode.pc_set_norm_form <- function(x) {
  encode(pc_set(x))
}

decode.coded_vec_pc_set_norm_form <- function(x) {
  lapply(decode(x, "pc_set"), pc_set_norm_form)
}

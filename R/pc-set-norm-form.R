#' @export
.pc_set_norm_form <- function(x, transposition = NULL) {
  checkmate::qassert(x, "N[0,12)")
  stopifnot(is.null(transposition) || checkmate::qtest(transposition, "N1"))
  stopifnot(!anyDuplicated(x))
  stopifnot(identical(x, sort(x)))
  class(x) <- c("pc_set_norm_form", "pc_set")
  attr(x, "transposition") <- transposition
  x
}

#' @export
pc_set_norm_form <- function(x) {
  UseMethod("pc_set_norm_form")
}

#' @export
pc_set_norm_form.default <- function(x) {
  pc_set_norm_form(pc_set(x))
}

#' @export
pc_set_norm_form.pc_set <- function(x) {
  pc_set_norm_form(pc_set_norm_order(x))
}

#' @export
pc_set_norm_form.pc_set_norm_order <- function(x) {
  x_num <- as.numeric(x)
  transposition <- x_num[1]
  res <- if (length(x_num) == 0) x_num else {
    (x_num - transposition) %% 12L
  }
  .pc_set_norm_form(res, - transposition)
}

#' @export
pc_set_norm_form.pc_set_norm_form <- function(x) x

#' @export
print.pc_set_norm_form <- function(x, ...) {
  trans <- get_transposition(x)
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

#' @export
get_transposition <- function(x) UseMethod("get_transposition")
#' @export
get_transposition.pc_set_norm_form <- function(x) attr(x, "transposition")

#' @export
encode.pc_set_norm_form <- function(x) {
  encode(pc_set(x))
}

decode.coded_vec_pc_set_norm_form <- function(x) {
  lapply(decode(x, "pc_set"), pc_set_norm_form)
}

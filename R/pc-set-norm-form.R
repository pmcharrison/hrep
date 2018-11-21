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
get_transposition <- function(x) UseMethod("get_transposition")
#' @export
get_transposition.pc_set_norm_form <- function(x) attr(x, "transposition")

#' @export
as.pc_set_norm_form <- function(x) UseMethod("as.pc_set_norm_form")
#' @export
as.pc_set_norm_form.numeric <- function(x) {
  as.pc_set_norm_form(pc_set(x))
}
#' @export
as.pc_set_norm_form.pc_set <- function(x) {
  as.pc_set_norm_form(as.pc_set_norm_order(x))
}
#' @export
as.pc_set_norm_form.pc_set_norm_order <- function(x) {
  x_num <- as.numeric(x)
  transposition <- x_num[1]
  res <- if (length(x_num) == 0) x_num else {
    (x_num - transposition) %% 12L
  }
  pc_set_norm_form(res, - transposition)
}

#' @export
pc_set_norm_form <- function(x, transposition = NULL) {
  class(x) <- "pc_set_norm_form"
  attr(x, "transposition") <- transposition
  x
}

#' @export
as.pc_set.pc_set_norm_form <- function(x) {
  pc_set(as.integer(x))
}

#' @export
encode.pc_set_norm_form <- function(x) {
  encode.pc_set(as.pc_set(x))
}

decode_pc_set_norm_form <- function(x) {
  lapply(decode_pc_set(x), pc_set_norm_form)
}

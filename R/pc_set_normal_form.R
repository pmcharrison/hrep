#' @export
print.pc_set_normal_form <- function(x, ...) {
  cat("Pitch-class set (normal form): ",
      paste0("[", paste(as.integer(x), collapse = ", "), "]\n"),
      sep = "")
  cat("Transposition from original pitch-class set: ",
      get_transposition(x), "\n",
      sep = "")
}

#' @export
get_transposition <- function(x) UseMethod("get_transposition")
#' @export
get_transposition.pc_set_normal_form <- function(x) attr(x, "transposition")

#' @export
get_pc_set_normal_form <- function(x) UseMethod("get_pc_set_normal_form")
#' @export
get_pc_set_normal_form.numeric <- function(x) {
  get_pc_set_normal_form(new_pc_set(x))
}
#' @export
get_pc_set_normal_form.pc_set <- function(x) {
  get_pc_set_normal_form(get_pc_set_normal_order(x))
}
#' @export
get_pc_set_normal_form.pc_set_normal_order <- function(x) {
  x_int <- as.integer(x)
  transposition <- x_int[1]
  res <- if (length(x_int) == 0) x_int else {
    (x_int - transposition) %% 12L
  }
  class(res <- "pc_set_normal_form")
  attr(res, "transposition") <- - transposition
  res
}

#' @export
representations <- c("fr_chord", "fr_sparse_spectrum",
                     "pi_chord", "pi_sparse_spectrum",
                     "pc_chord", "pc_chord_type",
                     "pc_set", "pc_set_norm_form", "pc_set_norm_order",
                     "pc_milne_spectrum")

#' @export
represent <- function(x, type) {
  UseMethod("represent")
}

#' @export
represent.default <- function(x, type) {
  checkmate::qassert(type, "S1")
  stopifnot(type %in% representations)
  get(type, mode = "function")(x)
}

#' @export
represent.corpus <- function(x, type) {
  f <- function(sym) represent(sym, type)
  transform_symbols(x, f, type)
}

#' @export
represent.vec <- function(x, type) {
  f <- function(sym) represent(sym, type)
  transform_symbols(x, f, type)
}

#' @export
represent.coded_vec <- function(x, type) {
  f <- function(sym) represent(sym, type)
  transform_symbols(x, f, type)
}

#' @export
represent.list <- function(x, type) {
  f <- function(sym) represent(sym, type)
  vec(
    x = purrr::map(x, f),
    type = type
  )
}

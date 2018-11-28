#' #' @export
#' infer_type <- function(x, ...) {
#'   UseMethod("infer_type")
#' }
#'
#' #' @export
#' infer_type.list <- function(x, ...) {
#'   types <- unique(purrr::map_chr(x, ~ type(.)[1]))
#'   if (length(types) == 0L) {
#'     as.character(NA)
#'   } else if (length(types) == 1L) {
#'     types
#'   } else {
#'     stop("cannot infer type of list containing multiple types (",
#'          paste(types, collapse = ", "), ")")
#'   }
#' }

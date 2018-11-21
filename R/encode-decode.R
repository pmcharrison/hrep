#' @export
code_seq <- function(x, type) {
  checkmate::qassert(x, "X")
  checkmate::qassert(type, "S1")
  x <- as.integer(x)
  attr(x, "type") <- type
  class(x) <- c("code_seq", "integer")
  x
}

#' @export
type <- function(x, ...) {
  UseMethod("type")
}

#' @export
type.code_seq <- function(x, ...) {
  attr(x, "type")
}

#' @export
is.code_seq <- function(x) {
  is(x, "code_seq")
}

#' @export
print.code_seq <- function(x, ...) {
  n <- length(x)
  type <- type(x)
  cat("Encoded sequence: Type = '", type, "', length = ", n, "\n")
}

#' @export
encode <- function(x, ...) {
  UseMethod("encode")
}

#' @export
encode.list <- function(x, ...) {
  type <- if (length(x) == 1L) "empty" else class(x[[1]])
  code_seq(purrr::map_int(x, encode),
           type = type)
}

decode <- function(x, type = NULL, ...) {
  if (is.null(type)) type <- type(x)
  if (is.null(type)) stop("couldn't infer the type of <x>")
  checkmate::qassert(type, "S1")
  f <- paste0("decode_", type)
  do.call(what = f, args = list(as.integer(x)))
}

decode_empty <- function(x) {
  list()
}

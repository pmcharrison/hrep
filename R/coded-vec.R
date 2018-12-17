#' Coded vector
#'
#' This function creates a coded vector.
#' Coded vectors are used to represent sequences of symbols
#' from a finite alphabet.
#' Each symbol should be coded as an integer using a bijective mapping.
#' @param x (Integer vector) The coded sequence.
#' @param type (Character scalar) Identifies the symbol type.
#' @param metadata (List) A (possibly-empty) list of metadata information.
#' @return An object of class \code{coded_vec}.
#' @export
coded_vec <- function(x, type, metadata = list()) {
  checkmate::qassert(x, "X")
  checkmate::qassert(type, "S1")
  checkmate::qassert(metadata, "l")
  x <- as.integer(x)
  class(x) <- c("coded_vec", "integer")
  type(x) <- type
  metadata(x) <- metadata
  x
}

#' @export
type.coded_vec <- function(x) {
  attr(x, "type")
}

`type<-.coded_vec` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' @export
metadata.coded_vec <- function(x) {
  attr(x, "metadata")
}

`metadata<-.coded_vec` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

#' @export
is.coded_vec <- function(x) {
  is(x, "coded_vec")
}

#' @export
print.coded_vec <- function(x, ...) {
  cat("Encoded vector of type '", type(x),
      "', length = ", num_symbols(x),
      if (length(metadata(x)) > 0L) " (metadata available)", "\n", sep = "")
}

#' @export
num_symbols.coded_vec <- function(x) length(x)

#' @export
encode <- function(x, ...) {
  UseMethod("encode")
}

#' @export
encode.vec <- function(x, ...) {
  coded_vec(purrr::map_int(x, encode),
            type = type(x),
            metadata = metadata(x))
}

#' @export
encode.coded_vec <- function(x, ...) x

#' @export
as.coded_vec <- function(x) UseMethod("as.coded_vec")

#' @export
as.coded_vec.vec <- function(x) encode(x)

#' @export
is.coded.coded_vec <- function(x) TRUE

#' @export
decode <- function(x, type = NULL, ...) {
  if (is.null(type)) type <- type(x)
  if (is.null(type)) stop("type needs to be provided before decoding")
  checkmate::qassert(type, "S1")
  f <- paste0("decode.coded_vec_", type)
  vec(do.call(what = f, args = list(as.integer(x))),
      type = type,
      metadata = metadata(x))
}

#' @export
transform_symbols.coded_vec <- function(x, f, type) {
  encode(transform_symbols(decode(x), f, type))
}

#' @export
is.empty.coded_vec <- function(x) length(x) == 0L

#' @export
`[.coded_vec` <- function(x, i) {
  coded_vec(as.integer(x)[i], type = type(x), metadata = metadata(x))
}

#' @export
`[<-.coded_vec` <- function(x, i, value) {
  checkmate::qassert(value, "X")
  NextMethod("[<-.corpus")
}

#' @export
`[[<-.coded_vec` <- function(x, i, value) {
  checkmate::qassert(value, "X1")
  NextMethod("[<-.corpus")
}

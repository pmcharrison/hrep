#' Coded vector
#'
#' This function creates a coded vector.
#' Coded vectors are used to represent sequences of symbols
#' from a finite alphabet.
#' Each symbol should be coded as an integer using a bijective mapping.
#' @param x (Integer vector) The coded sequence.
#' @param type (Character scalar) Identifies the symbol \code{\link{type}}.
#' @param metadata (List) A (possibly-empty) list of metadata information.
#' @return An object of class "coded_vec".
#' @seealso \code{\link{vec}}.
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

#' @export
`metadata<-.coded_vec` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

#' Type-checking for "coded_vec"
#'
#' Checks whether an object is of type "coded_vec".
#' @param x Object to check.
#' @return Logical scalar.
#' @export
is.coded_vec <- function(x) {
  is(x, "coded_vec")
}



#' @export
print.coded_vec <- function(x, ...) {
  cat("Encoded vector of type '", type(x),
      "', length = ", num_elements(x),
      if (length(metadata(x)) > 0L) " (metadata available)", "\n", sep = "")
}

#' @rdname num_elements
#' @export
num_elements.coded_vec <- function(x) length(x)

#' Encode
#'
#' Transforms a given object into an integer-based encoding.
#'
#' @param x Object to transform.
#' @return Encoded object.
#'
#' @details
#' Encoding is currently defined for the following types:
#' * \code{\link{pc_set}}
#' * \code{\link{pc_chord}}
#' * \code{\link{pc_chord_type}}
#'
#' An error will be thrown when trying to encode objects of other types.
#' @md
#' @seealso \code{\link{decode}} for the reverse operation.
#' @export
encode <- function(x) {
  UseMethod("encode")
}

#' @rdname encode
#' @export
encode.vec <- function(x) {
  coded_vec(purrr::map_int(x, encode),
            type = type(x),
            metadata = metadata(x))
}

#' @rdname encode
#' @export
encode.coded_vec <- function(x) x

#' @rdname is.coded
#' @export
is.coded.coded_vec <- function(x) TRUE


#' Decode
#'
#' Decodes an object from an integer-based encoding.
#' @param x Object to decode.
#' @seealso \code{\link{encode}} for the reverse operation.
#' @rdname decode
#' @export
decode <- function(x, x_type = type(x)) {
  checkmate::qassert(x, "X")
  f <- paste0("decode.coded_vec_", x_type)
  vec(do.call(f, args = list(x)),
      type = x_type,
      metadata = metadata(x))
}

#' @rdname transform_symbols
#' @export
transform_symbols.coded_vec <- function(x, f, type, ...) {
  encode(transform_symbols(decode(x), f, type = type, ...))
}

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

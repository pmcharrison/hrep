#' Vector
#'
#' This function creates an object of the class "vec".
#'
#' "vec" objects are typed vectors of chord symbols.
#' Their underlying representation is a list,
#' where every list element is of a specified type (see \code{\link{type}}).
#'
#' @param x List of (un-encoded) chord symbols, each of class \code{type}.
#' @param type (Character scalar) Identifies the symbol \code{\link{type}}.
#' @param metadata (List) A (possibly-empty) list of metadata information.
#'
#' @return An object of class "vec".
#'
#' @rdname vec
#' @seealso \code{\link{coded_vec}}, code{\link{encode}}, code{\link{decode}}.
#' @export
vec <- function(x, type, metadata = list()) {
  checkmate::qassert(x, "l")
  checkmate::qassert(type, "S1")
  checkmate::qassert(metadata, "l")
  if (!all(purrr::map_lgl(x, ~ is(., type))))
    stop("not all elements of <x> were of type ", type)
  class(x) <- c("vec", "list")
  type(x) <- type
  metadata(x) <- metadata
  x
}

#' @export
type.vec <- function(x) {
  attr(x, "type")
}

`type<-.vec` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' @export
metadata.vec <- function(x) {
  attr(x, "metadata")
}

`metadata<-.vec` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

#' Check for class "vec"
#'
#' Checks whether an object is of class "vec".
#' @param x Object to check.
#' @return Logical scalar.
#' @export
is.vec <- function(x) {
  is(x, "vec")
}

#' @export
as.list.vec <- function(x, ...) {
  class(x) <- "list"
  attributes(x) <- NULL
  x
}

#' @export
is.coded.vec <- function(x) FALSE

#' @export
print.vec <- function(x, ...) {
  cat("Vector of type '", type(x),
      "', length = ", num_elements(x),
      if (length(metadata(x)) > 0L) " (metadata available)", "\n", sep = "")
}

#' @param annotate An optional character vector of the same length
#' as \code{x}, with which to annotate each element.
#' @rdname view
#' @export
view.vec <- function(x, annotate = NULL, ...) {
  if (length(x) > 200) stop("cannot view a vector this long")
  abcR::view_pi_chord_seq(as.list(transform_symbols(x, pi_chord, "pi_chord")),
                          annotate = annotate,
                          ...)
}

#' @rdname num_elements
#' @export
num_elements.vec <- function(x) length(x)

#' @rdname transform_symbols
#' @export
transform_symbols.vec <- function(x, f, type, ...) {
  stopifnot(is.function(f))
  checkmate::qassert(type, "S1")
  vec(
    x = purrr::map(as.list(x), f),
    type = type,
    metadata = metadata(x)
  )
}

#' @export
`[.vec` <- function(x, i) {
  vec(as.list(x)[i], type = type(x), metadata = metadata(x))
}

#' @export
`[<-.vec` <- function(x, i, value) {
  target_type <- type(x)
  if (!all(purrr::map_lgl(x, ~ is(., target_type))))
    stop("not all elements of <x> were of type ", target_type)
  NextMethod("[<-.corpus")
}

#' @export
`[[<-.vec` <- function(x, i, value) {
  if (!is(value, type(x)))
    stop("new value was not of type ", type(x))
  NextMethod("[<-.corpus")
}

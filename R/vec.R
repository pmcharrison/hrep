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
as.vec.coded_vec <- function(x) decode(x)

#' @export
is.coded.vec <- function(x) FALSE

#' @export
print.vec <- function(x, ...) {
  cat("Vector of type '", type(x),
      "', length = ", num_symbols(x),
      if (length(metadata(x)) > 0L) " (metadata available)", "\n", sep = "")
}

#' @export
view.vec <- function(x, ...) {
  if (length(x) > 200) stop("cannot view a vector this long")
  abcR::view_pi_chord_seq(as.list(transform_symbols(x, pi_chord)), ...)
}

#' @export
num_symbols.vec <- function(x) length(x)

#' @export
transform_symbols.vec <- function(x, f, type) {
  stopifnot(is.function(f))
  checkmate::qassert(type, "S1")
  for (i in seq_along(x)) {
    tmp <- f(x[[i]])
    if (!is(tmp, type))
      stop("transformed symbol had invalid type ",
           "(requested = ", type, ", ",
           "returned = ", class(tmp), ")")
    x[[i]] <- tmp
  }
  type(x) <- type
  x
}

#' @export
is.empty.vec <- function(x) length(x) == 0L

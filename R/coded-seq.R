#' @export
coded_seq <- function(x, type, description = NULL) {
  checkmate::qassert(x, "X")
  checkmate::qassert(type, "S1")
  stopifnot(is.null(description) || checkmate::qtest(description, "S1"))
  x <- as.integer(x)
  class(x) <- c("coded_seq", "integer")
  type(x) <- type
  description(x) <- description
  x
}

#' @export
type.coded_seq <- function(x) {
  attr(x, "type")
}

`type<-.coded_seq` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' @export
description.coded_seq <- function(x) {
  attr(x, "description")
}

`description<-.coded_seq` <- function(x, value) {
  attr(x, "description") <- value
  x
}

#' @export
is.coded_seq <- function(x) {
  is(x, "coded_seq")
}

#' @export
print.coded_seq <- function(x, ...) {
  n <- length(x)
  type <- type(x)
  desc <- description(x)
  cat("Encoded sequence of type '", type,
      "', length = ", num_symbols(x), "\n", sep = "")
  if (!is.null(desc)) {
    cat(strwrap(paste0("Description: '", desc, "'")), "\n\n")
  }
}

#' @export
num_symbols.coded_seq <- function(x) length(x)

#' @export
encode <- function(x, ...) {
  UseMethod("encode")
}

#' @export
encode.list <- function(x, ...) {
  type <- if (length(x) == 0L) "empty" else class(x[[1]])
  coded_seq(purrr::map_int(x, encode), type = type)
}

#' @export
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

#' @export
transform_symbols.coded_seq <- function(x, f) {
  description <- description(x)
  decoded <- decode(x)
  transformed <- lapply(decoded, f)
  res <- encode(transformed)
  description(res) <- description
  res
}

# normalise_bass.harmony_composition <- function(x) {
#   chords_int <- as.integer(x)
#   chords_decoded <- decode_chords(chords_int)
#   chords_normalised <- lapply(chords_decoded, normalise_bass.chord)
#   new_harmony_composition(chords_normalised)
# }

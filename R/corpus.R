# Creation ####

#' Corpus
#'
#' Creates \code{corpus} objects,
#' which comprise ordered sets of (possibly coded) vectors,
#' each describing a sequence of discrete symbols.
#' @param x List of sequences, where each sequence is an object of class
#' \code{vec} or \code{coded_vec}.
#' @param type (Character scalar) Symbol type.
#' @param metadata (List) List of metadata information.
#' @return An object of class \code{corpus}.
#' @export
corpus <- function(x, type, metadata = list()) {
  checkmate::qassert(x, "l")
  checkmate::qassert(metadata, "l")
  checkmate::qassert(type, "S1")
  cl <- if (all(purrr::map_lgl(x, is.coded_vec)))
    "coded_vec" else if (all(purrr::map_lgl(x, is.vec)))
      "vec" else stop("every element of <x> must be an object of class ",
                      "'coded_vec' or 'vec'")
  if (!all(type == purrr::map_chr(x, function(y) type(y))))
    stop("not all sequences were of type '", type, "'")
  class(x) <- c("corpus", "list")
  type(x) <- type
  metadata(x) <- metadata
  attr(x, "coded") <- cl == "coded_vec"
  x
}

#' @export
as.list.corpus <- function(x, ...) {
  attributes(x) <- NULL
  x
}

# Subsetting ####
#' @export
`[.corpus` <- function(x, i) {
  corpus(as.list(x)[i], type = type(x), metadata = metadata(x))
}
#' @export
`[<-.corpus` <- function(x, i, value) {
  # We perform some sanity checks before allowing the assignment
  value <- corpus(x = as.list(value), type = type(x))
  if (!(is.coded(x) == is.coded(value)))
    stop("old corpus and new value must either be both uncoded or both coded")
  value <- as.list(value)
  NextMethod("[<-.corpus")
}
#' @export
`[[<-.corpus` <- function(x, i, value) {
  x[i] <- list(value)
  x
}

# Combination ####
#' @export
c.corpus <- function(...) {
  x <- list(...)
  types <- unique(purrr::map_chr(x, type))
  if (length(types) > 1L) stop("cannot combine corpora of different types")
  type <- types
  corpus(do.call(c, lapply(x, as.list)), type = type)
}

# Properties ####
#' @export
num_sequences.corpus <- function(x) length(x)
#' @export
num_symbols.corpus <- function(x) {
  sum(vapply(x, num_symbols, integer(1)))
}

#' @export
metadata.corpus <- function(x) attr(x, "metadata")

#' @export
`metadata<-.corpus` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

#' @export
type.corpus <- function(x) {
  attr(x, "type")
}

`type<-.corpus` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' @export
is.coded.corpus <- function(x) attr(x, "coded")

#' @export
encode.corpus <- function(x, ...) {
  if (!is.coded(x)) {
    for (i in seq_along(x)) x[[i]] <- encode(x[[i]])
    attr(x, "coded") <- TRUE
  }
  x
}

#' @export
decode.corpus <- function(x) {
  if (is.coded(x)) {
    for (i in seq_along(x)) x[[i]] <- decode(x[[i]])
    attr(x, "coded") <- FALSE
  }
  x
}

# Display ####
#' @export
print.corpus <- function(x, ...) {
  n <- num_sequences(x)
  N <- num_symbols(x)
  cat("\nA corpus of", n , ngettext(n, "sequence", "sequences"), "\n")
  cat("  total size =", N, ngettext(N, "symbol", "symbols"), "\n")
  cat("  symbol type = '", type(x), "'\n", sep = "")
  cat("  coded =", tolower(is.coded(x)), "\n")
  if (length(metadata(x)) > 0L) cat(" (Metadata available)", "\n")
  cat("\n")
}

#' @export
transform_symbols.corpus <- function(x, f, type) {
  stopifnot(is.function(f))
  checkmate::qassert(type, "S1")
  for (i in num_sequences(x)) {
    x[[i]] <- transform_symbols(x[[i]], f, type)
  }
  type(x) <- type
  x
}

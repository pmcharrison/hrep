# Creation ####
#' @export
corpus <- function(x, description = NULL, type = NULL) {
  checkmate::qassert(x, "l")
  stopifnot(is.null(description) || checkmate::qtest(description, "S1"))
  stopifnot(is.null(type) || checkmate::qtest(type, "S1"))
  if (!purrr::map_lgl(x, is.coded_vec))
    stop("every element of <x> must be an object of class 'coded_vec'")
  types <- unique(purrr::map_chr(x, function(x) type(x)))
  if (length(types) > 1L)
    stop("every element of <x> must be a coded sequence of the same type ",
         "(observed: ", paste(types, collapse = ", "), ")")
  if (!is.null(type) && !types == type)
    stop("requested type (", type, ") inconsistent with observed type (",
         types, ")")
  class(x) <- c("corpus", "list")
  type(x) <- type
  description(x) <- description
  x
}

#' @export
as.list.corpus <- function(x) {
  class(x) <- "list"
  x
}

# Subsetting ####
#' @export
`[.corpus` <- function(x, i) {
  res <- new_corpus(as.list(x)[i],
                            description = paste(description(x), "(subset)"))
  if (num_compositions(res) == 1) {
    description(res) <- description(res[[1]])
  }
  res
}
#' @export
`[<-.corpus` <- function(x, i, value) {
  stop("Assignment with [ ] not valid for corpus objects.\n",
       "You can update individual compositions with the [[ ]] operator, however.")
}

# Combination ####
#' @export
c.corpus <- function(...) {
  new_corpus(do.call(c, lapply(list(...), as.list)),
                     description = "Combined corpora")
}

# Properties ####
#' @export
num_sequences.corpus <- function(x) length(x)
#' @export
num_symbols.corpus <- function(x) {
  sum(vapply(x, num_symbols, integer(1)))
}

#' @export
description.corpus <- function(x) attr(x, "description")

#' @export
`description<-.corpus` <- function(x, value) {
  attr(x, "description") <- value
  x
}

type.corpus <- function(x) {
  attr(x, "type")
}

`type<-.corpus` <- function(x, value) {
  attr(x, "type") <- value
  x
}


# Display ####
#' @export
print.corpus <- function(x, ...) {
  desc <- description(x)
  cat("\n\tA corpus of encoded sequences\n\n")
  if (!is.null(desc))
    cat(strwrap(paste0("'", desc, "'\n")))
  cat("Num. sequences =", num_sequences(x), "\n")
  cat("Num. symbols =", num_symbols(x), "\n\n")
}

#' @export
transform_symbols.corpus <- function(x, f) {
  for (i in num_sequences(x)) {
    x[[i]] <- transform_symbols(x[[i]], f)
  }
  x
}

#' # Other ####
#' #' @export
#' normalise_bass.corpus <- function(x) {
#'   new_corpus(lapply(as.list(x), normalise_bass))
#' }

#' # Coercion ####
#' #' @export
#' as.list.harmony_corpus <- function(x) {
#'   class(x) <- "list"
#'   x
#' }
#' #' @export
#' as.harmony_corpus <- function(x) UseMethod("as.harmony_corpus")
#' #' @export
#' as.harmony_corpus.harmony_corpus <- function(x) x
#' #' @export
#' as.harmony_corpus.list <- function(x) new_harmony_corpus.list(x)

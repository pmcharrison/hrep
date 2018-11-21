# Creation ####
#' @export
new_harmony_corpus <- function(x, description = NULL) {
  UseMethod("new_harmony_corpus")
}

#' @export
new_harmony_corpus.list <- function(x, description = NULL) {
  for (i in seq_along(x)) {
    x[[i]] <- as.harmony_composition(x[[i]])
  }
  class(x) <- "harmony_corpus"
  description(x) <- description
  x
}

# Coercion ####
#' @export
as.list.harmony_corpus <- function(x) {
  class(x) <- "list"
  x
}
#' @export
as.harmony_corpus <- function(x) UseMethod("as.harmony_corpus")
#' @export
as.harmony_corpus.harmony_corpus <- function(x) x
#' @export
as.harmony_corpus.list <- function(x) new_harmony_corpus.list(x)

# Subsetting ####
#' @export
`[.harmony_corpus` <- function(x, i) {
  res <- new_harmony_corpus(as.list(x)[i],
                            description = paste(description(x), "(subset)"))
  if (num_compositions(res) == 1) {
    description(res) <- description(res[[1]])
  }
  res
}
#' @export
`[<-.harmony_corpus` <- function(x, i, value) {
  stop("Assignment with [ ] not valid for harmony_corpus objects.\n",
       "You can update individual compositions with the [[ ]] operator, however.")
}

# Combination ####
#' @export
c.harmony_corpus <- function(...) {
  new_harmony_corpus(do.call(c, lapply(list(...), as.list)),
                     description = "Combined corpora")
}

# Properties ####
#' @export
num_compositions.harmony_corpus <- function(x) length(x)
#' @export
num_events.harmony_corpus <- function(x) {
  sum(vapply(x, num_events, integer(1)))
}

#' @export
description.harmony_corpus <- function(x) attr(x, "description")
`description<-.harmony_corpus` <- function(x, value) {
  attr(x, "description") <- value
  x
}

# Display ####
#' @export
print.harmony_corpus <- function(x, ...) {
  desc <- description(x)
  cat("\n")
  cat("\tA harmony corpus")
  cat("\n\n")
  if (is.null(desc)) cat("(No description found)") else {
    cat(strwrap(paste0("'", desc, "'")))
  }
  cat("\n")
  cat("Num. compositions =", num_compositions(x))
  cat("\n")
  cat("Num. events =", num_events(x))
  cat("\n\n")
}

# Other ####
#' @export
normalise_bass.harmony_corpus <- function(x) {
  new_harmony_corpus(lapply(as.list(x), normalise_bass))
}
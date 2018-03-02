# Creation ####
#' @export
new_harmony_corpus <- function(x) UseMethod("new_harmony_corpus")
#' @export
new_harmony_corpus.list <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- as.harmony_composition(x[[i]])
  }
  class(x) <- "harmony_corpus"
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
  new_harmony_corpus(as.list(x)[i])
}
#' @export
`[<-.harmony_corpus` <- function(x, i, value) {
  stop("Assignment with [ ] not valid for harmony_corpus objects.\n",
       "You can update individual compositions with the [[ ]] operator, however.")
}

# Combination ####
#' @export
c.harmony_corpus <- function(...) {
  new_harmony_corpus(do.call(c, lapply(list(...), as.list)))
}

# Properties ####
#' @export
num_compositions.harmony_corpus <- function(x) length(x)
#' @export
num_events.harmony_corpus <- function(x) {
  sum(vapply(x, num_events, integer(1)))
}

# Display ####
#' @export
print.harmony_corpus <- function(x, ...) {
  cat("---\n")
  cat("A harmony corpus\n\n")
  cat("Num. compositions =", num_compositions(x), "\n")
  cat("Num. events =", num_events(x), "\n")
  cat("---\n")
}

# Other ####
#' @export
normalise_bass.harmony_corpus <- function(x) {
  new_harmony_corpus(lapply(as.list(x), normalise_bass))
}

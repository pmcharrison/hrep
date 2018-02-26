# Display ####

#' @export
setMethod(
  "show", signature(object = "harmony_corpus"),
  function(object) {
    cat("---\n")
    cat("A harmony corpus\n\n")
    cat("Num. compositions =", num_compositions(object), "\n")
    cat("Num. events =", num_events(object), "\n")
    cat("---\n")
  }
)

# Creation ####

# Coercion ####
setMethod("as.harmony_corpus", signature(x = "harmony_corpus"), function(x) x)
setMethod(
  "as.harmony_corpus", signature(x = "list"),
  function(x) {
    for (i in seq_along(x)) {
      x[[i]] <- as.harmony_composition(x[[i]])
    }
    new("harmony_corpus", compositions = x)
  }
)
#' @export
setMethod(
  "as.list", signature(x = "harmony_corpus"),
  function(x, ...) x@compositions
)

# Combination ####

#' @export
combine_corpora <- function(...) {
  x <- list(...)
  assertthat::assert_that(
    is.list(x),
    all(sapply(x, function(y) is(y, "harmony_corpus")))
  )
  lapply(x, function(y) y@compositions) %>%
    (function(z) do.call(c, z)) %>%
    as.harmony_corpus
}

# Properties ####

setMethod("num_compositions", signature(x = "harmony_corpus"),
          function(x) length(x@compositions))
setMethod("num_events", signature(x = "harmony_corpus"),
          function(x) {
            sum(vapply(x@compositions, num_events, integer(1)))
          })

# Cruft ####

#' # export
# setGeneric("declass", function(x) standardGeneric("declass"))
# setMethod("declass", signature(x = "harmony_corpus"),
#           function(x) {
#             l <- x@compositions
#             for (i in seq_along(l)) {
#               l[[i]] <- declass(l[[i]])
#             }
#             l
#           })

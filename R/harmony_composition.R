# Display ####

#' @export
setMethod(
  "show", signature(object = "harmony_composition"),
  function(object) {
    cat("---\n")
    cat("A harmony composition\n\n")
    cat("Num. events =", num_events(object), "\n")
    cat("---\n")
  }
)

# Coercion ####

#' @export
setMethod(
  "as.integer",
  signature(x = "harmony_composition"),
  function(x, ...) x@events
)
setMethod("as.harmony_composition",
          signature(x = "harmony_composition"),
          function(x) x)
setMethod("as.harmony_composition",
          signature(x = "numeric"),
          function(x) {
            if (!all(x == round(x))) {
              stop("All elements of <x> must be integers")
            }
            new("harmony_composition", events = as.integer(x))
          })
setMethod("as.harmony_composition",
          signature(x = "list"),
          function(x) {
            if (!all(sapply(x, HarmonyUtils::is.chord))) {
              stop("All elements of <x> must be chords")
            }
            x %>% (HarmonyUtils::encode_chords) %>% as.harmony_composition
          })

# Properties ####
setMethod("num_events",
          signature(x = "harmony_composition"),
          function(x) length(x@events))

# Cruft ####

# setMethod("declass", signature(x = "harmony_composition"),
#           function(x) as.integer(x))

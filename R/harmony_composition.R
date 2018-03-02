# Creation ####

# harmony_composition comprises a series of chords coded as integers
# with respect to chord_alphabet.

#' @export
new_harmony_composition <- function(x) {
  UseMethod("new_harmony_composition")
}
#' @export
new_harmony_composition.numeric <- function(x) {
  x <- as.integer(x)
  class(x) <- "harmony_composition"
  x
}

#' @export
new_harmony_composition.list <- function(x) {
  y <- encode_chords(x)
  class(y) <- "harmony_composition"
  y
}

# Coercion ####
#' @export
as.harmony_composition <- function(x, safe = TRUE) {
  UseMethod("as.harmony_composition")
}
#' @export
as.harmony_composition.harmony_composition <- function(x) x
#' @export
as.harmony_composition.numeric <- function(x) {
  new_harmony_composition.numeric(x)
}
#' @export
as.harmony_composition.list <- function(x) {
  new_harmony_composition.list(x)
}

# Properties ####
num_events.harmony_composition <- function(x) length(x)

# Display ####

#' @export
print.harmony_composition <- function(x, ...) {
  cat("---\n")
  cat("A harmony composition\n\n")
  cat("Num. events =", num_events(x), "\n")
  cat("---\n")
}

# Other ####
#' @export
num_events.harmony_composition <- function(x) length(x)
#' @export
normalise_bass.harmony_composition <- function(x) {
  chords_int <- as.integer(x)
  chords_decoded <- decode_chords(chords_int)
  chords_normalised <- lapply(chords_decoded, normalise_bass.chord)
  new_harmony_composition(chords_normalised)
}

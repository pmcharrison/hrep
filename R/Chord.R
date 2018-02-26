#' @include classes.R

#' @export
setMethod(
  "show", signature(object = "chord"),
  function(object) {
    cat("Chord: ",
        "[", get_bass_pc(object), "] ",
        paste(get_non_bass_pc_set(object), collapse = " "),
        sep = "")
  }
)

#' @export
setMethod(
  "as.integer", signature(x = "chord"),
  function(x, ...) {
    c(
      48L + get_bass_pc(x),
      60L + get_non_bass_pc_set(x)
    )
  }
)

#' @export
setGeneric("as.chord", function(x) standardGeneric("as.chord"))
setMethod("as.chord", signature(x = "numeric"),
          function(x) {
            assertthat::assert_that(length(x) > 0)
            y <- x %% 12
            new_chord(y[1], y[-1])
          })
setMethod("as.chord", signature(x = "chord"), function(x) x)

#' @param bass_pc Integer scalar corresponding to bass pitch class
#' @param pc_set Integer vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
new_chord <- function(bass_pc, pc_set) {
  assertthat::assert_that(
    is.numeric(pc_set),
    all(pc_set == round(pc_set)),
    all(pc_set >= 0 & pc_set < 12),
    !anyDuplicated(pc_set),
    is.numeric(bass_pc),
    bass_pc == round(bass_pc),
    bass_pc >= 0,
    bass_pc < 12,
    length(bass_pc) == 1
  )
  bass_pc <- as.integer(bass_pc)
  pc_set <- as.integer(pc_set)
  non_bass_pc_set <- setdiff(pc_set, bass_pc) %>% sort
  new(
    "chord",
    bass_pc = bass_pc,
    non_bass_pc_set = non_bass_pc_set
  )
}

setGeneric("get_bass_pc",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_bass_pc")
           })
#' @export
setMethod("get_bass_pc", signature(x = "chord"), function(x) x@bass_pc)


setGeneric("get_non_bass_pc_set",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_non_bass_pc_set")
           })
#' @export
setMethod("get_non_bass_pc_set", signature(x = "chord"), function(x) x@non_bass_pc_set)

setGeneric("get_pc_set",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_pc_set")
           })
#' @export
setMethod("get_pc_set", signature(x = "chord"), function(x) sort(c(get_bass_pc(x),
                                                                   get_non_bass_pc_set(x))))
#' @export
setGeneric("transpose", function(x, interval) standardGeneric("transpose"))
setMethod(
  "transpose", signature(x = "pc_set"),
  function(x, interval) {
    assertthat::assert_that(
      assertthat::is.scalar(interval),
      is.numeric(interval),
      interval == round(interval)
    )
    interval <- as.integer(interval)
    x@pc <- sort((x@pc + interval) %% 12L)
    x
  }
)
setMethod(
  "transpose", signature(x = "chord"),
  function(x, interval) {
    assertthat::assert_that(
      assertthat::is.scalar(interval),
      is.numeric(interval),
      interval == round(interval)
    )
    interval <- as.integer(interval)
    x@bass_pc <- (x@bass_pc + interval) %% 12L
    x@non_bass_pc_set <- sort((x@non_bass_pc_set + interval) %% 12L)
    x
  })

setGeneric("normalise_bass", function(x) standardGeneric("normalise_bass"))
#' @export
setMethod(
  "normalise_bass", signature(x = "chord"),
  function(x) {
    transpose(x, - get_bass_pc(x))
  }
)
setMethod(
  "normalise_bass", signature(x = "harmony_composition"),
  function(x) {
    x %>% as.integer %>% decode_chords %>% lapply(normalise_bass) %>%
      as.harmony_composition
  }
)
setMethod(
  "normalise_bass", signature(x = "harmony_corpus"),
  function(x) {
    x@compositions <- lapply(x@compositions, normalise_bass)
    x
  }
)

setGeneric("get_transpositions", function(x) standardGeneric("get_transpositions"))
#' @export
setMethod(
  "get_transpositions", signature(x = "chord"),
  function(x) {
    ref <- normalise_bass(x)
    lapply(0:11, function(int) transpose(ref, int))
  }
)

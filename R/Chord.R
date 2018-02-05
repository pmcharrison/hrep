#' @export
setClass(
  "Chord",
  slots = list(
    bass_pc = "integer",
    non_bass_pc_set = "integer"
  )
)

#' @export
setMethod(
  "print", signature(x = "Chord"),
  function(x, ...) {
    cat("Chord: ",
        "[", get_bass_pc(x), "] ",
        paste(get_non_bass_pc_set(x), collapse = " "),
        sep = "")
  }
)

#' @param bass_pc Integer scalar corresponding to bass pitch class
#' @param pc_set Integer vector corresponding to pitch-class set, may optionally include the bass pitch class
#' @export
make_chord <- function(bass_pc, pc_set) {
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
    "Chord",
    bass_pc = bass_pc,
    non_bass_pc_set = non_bass_pc_set
  )
}

#' @export
setGeneric("get_bass_pc",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_bass_pc")
           })
setMethod("get_bass_pc", signature(x = "Chord"), function(x) x@bass_pc)


#' @export
setGeneric("get_non_bass_pc_set",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_non_bass_pc_set")
           })
setMethod("get_non_bass_pc_set", signature(x = "Chord"), function(x) x@non_bass_pc_set)

#' @export
setGeneric("get_pc_set",
           valueClass = "integer",
           function(x) {
             standardGeneric("get_pc_set")
           })
setMethod("get_pc_set", signature(x = "Chord"), function(x) c(get_bass_pc(x),
                                                              get_non_bass_pc_set(x)))

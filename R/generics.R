# Coercion ####
#' @export
setGeneric("as.harmony_corpus",
           function(x) standardGeneric("as.harmony_corpus"))
#' @export
setGeneric("as.harmony_composition",
           function(x) standardGeneric("as.harmony_composition"))
#' @export
setGeneric("as.chord", function(x) standardGeneric("as.chord"))
#' @export
setGeneric("as.pc_set", function(x) standardGeneric("as.pc_set"))

# Properties ####
#' @export
setGeneric("num_compositions", function(x) standardGeneric("num_compositions"))
#' @export
setGeneric("num_events", function(x) standardGeneric("num_events"))
#' @export
setGeneric("get_bass_pc", valueClass = "integer",
           function(x) standardGeneric("get_bass_pc"))
#' @export
setGeneric("get_pc_set", valueClass = "integer",
           function(x)standardGeneric("get_pc_set"))
#' @export
setGeneric("get_transpositions",
           function(x) standardGeneric("get_transpositions"))
setGeneric("get_non_bass_pc_set", valueClass = "integer",
           function(x) standardGeneric("get_non_bass_pc_set"))

# Actions ####
#' @export
setGeneric("normalise_bass", function(x) standardGeneric("normalise_bass"))
#' @export
setGeneric("transpose", function(x, interval) standardGeneric("transpose"))

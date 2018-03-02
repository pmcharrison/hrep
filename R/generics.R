# Properties ####
#' @export
setGeneric("num_compositions", function(x) standardGeneric("num_compositions"))
#' @export
num_events <- function(x) UseMethod("num_events")
#' @export
setGeneric("get_bass_pc", valueClass = "integer",
           function(x) standardGeneric("get_bass_pc"))
#' @export
setGeneric("get_pc_set", valueClass = "integer",
           function(x)standardGeneric("get_pc_set"))
#' @export
setGeneric("get_transpositions",
           function(x) standardGeneric("get_transpositions"))
#' @export
setGeneric("get_non_bass_pc_set", valueClass = "integer",
           function(x) standardGeneric("get_non_bass_pc_set"))
#' @export
setGeneric("get_pc_set_storage_key", valueClass = "character",
           function(pc_set) standardGeneric("get_pc_set_storage_key"))
#' @export
setGeneric("get_pc_set_normal_order",
           function(x) standardGeneric("get_pc_set_normal_order"),
           valueClass = "pc_set_normal_order")

# Actions ####
#' @export
setGeneric("normalise_bass", function(x) standardGeneric("normalise_bass"))
#' @export
setGeneric("transpose", function(x, interval) standardGeneric("transpose"))

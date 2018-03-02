# Properties ####
#' @export
num_compositions <- function(x) UseMethod("num_compositions")
#' @export
num_events <- function(x) UseMethod("num_events")

#' @export
setGeneric("get_pc_set_normal_order",
           function(x) standardGeneric("get_pc_set_normal_order"),
           valueClass = "pc_set_normal_order")

# Actions ####
#' @export
setGeneric("normalise_bass", function(x) standardGeneric("normalise_bass"))
#' @export
setGeneric("transpose", function(x, interval) standardGeneric("transpose"))

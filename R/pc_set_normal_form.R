#' @export
setMethod(
  "as.integer", signature(x = "pc_set_normal_form"),
  function(x, ...) x@pc
)

#' @export
setMethod(
  "as.numeric", signature(x = "pc_set_normal_form"),
  function(x, ...) as.numeric(as.integer(x))
)

#' @export
setMethod(
  "show", signature(object = "pc_set_normal_form"),
  function(object) {
    cat("Pitch-class set (normal form): ",
        paste0("[", paste(as.integer(object), collapse = ", "), "]\n"),
        sep = "")
    cat("Transposition from original pitch-class set: ",
        object@shift, "\n",
        sep = "")
  }
)

#' @export
setGeneric("get_shift", function(x) standardGeneric("get_shift"))
setMethod("get_shift", signature(x = "pc_set_normal_form"),
          function(x) x@shift)

#' @export
setGeneric("get_pc_set_normal_form", function(x) standardGeneric("get_pc_set_normal_form"),
           valueClass = "pc_set_normal_form")
setMethod(
  "get_pc_set_normal_form", signature(x = "numeric"),
  function(x) x %>% make_pc_set %>% get_pc_set_normal_form
)
setMethod(
  "get_pc_set_normal_form", signature(x = "pc_set"),
  function(x) x %>% get_pc_set_normal_order %>% get_pc_set_normal_form
)
setMethod(
  "get_pc_set_normal_form", signature(x = "pc_set_normal_order"),
  function(x) {
    x_int <- as.integer(x)
    shift <- x_int[1]
    res <- if (length(x_int) == 0) x_int else (x_int - shift) %% 12L
    new("pc_set_normal_form", pc = res, shift = - shift)
  }
)

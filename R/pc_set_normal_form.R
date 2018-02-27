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
        object@transposition, "\n",
        sep = "")
  }
)

#' @export
setGeneric("get_transposition", function(x) standardGeneric("get_transposition"))
setMethod("get_transposition", signature(x = "pc_set_normal_form"),
          function(x) x@transposition)

#' @export
setGeneric("get_pc_set_normal_form", function(x) standardGeneric("get_pc_set_normal_form"),
           valueClass = "pc_set_normal_form")
setMethod(
  "get_pc_set_normal_form", signature(x = "numeric"),
  function(x) x %>% new_pc_set %>% get_pc_set_normal_form
)
setMethod(
  "get_pc_set_normal_form", signature(x = "pc_set"),
  function(x) x %>% get_pc_set_normal_order %>% get_pc_set_normal_form
)
setMethod(
  "get_pc_set_normal_form", signature(x = "pc_set_normal_order"),
  function(x) {
    x_int <- as.integer(x)
    transposition <- x_int[1]
    res <- if (length(x_int) == 0) x_int else (x_int - transposition) %% 12L
    new("pc_set_normal_form", pc = res, transposition = - transposition)
  }
)

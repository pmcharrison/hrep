setClass(
  "pc_set",
  slots = list(pc = "integer")
)

#' @export
setMethod(
  "as.integer", signature(x = "pc_set"),
  function(x, ...) x@pc
)

#' @export
setMethod(
  "as.numeric", signature(x = "pc_set"),
  function(x, ...) as.numeric(as.integer(x))
)

#' @export
setMethod(
  "show", signature(object = "pc_set"),
  function(object) {
    cat("Pitch-class set: ",
        paste(as.integer(object), collapse = " "),
        sep = "")
  }
)

#' @export
make_pc_set <- function(pitch_classes) {
  assertthat::assert_that(
    is.numeric(pitch_classes),
    all(pitch_classes == round(pitch_classes)),
    all(pitch_classes >= 0 & pitch_classes < 12),
    !anyDuplicated(pitch_classes)
  )
  new(
    "pc_set",
    pc = sort(as.integer(pitch_classes))
  )
}

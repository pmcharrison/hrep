#' @export
new_pc_set <- function(pitch_classes) {
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

setMethod("as.pc_set", signature(x = "pc_set"), function(x) x)

setMethod("as.pc_set", signature(x = "numeric"),
          function(x) new_pc_set(x))

setMethod(
  "as.integer", signature(x = "pc_set"),
  function(x, ...) x@pc
)

setMethod(
  "as.numeric", signature(x = "pc_set"),
  function(x, ...) as.numeric(as.integer(x))
)

setMethod(
  "show", signature(object = "pc_set"),
  function(object) {
    cat("Pitch-class set: ",
        paste0("[", paste(as.integer(object), collapse = ", "), "]"),
        sep = "")
  }
)

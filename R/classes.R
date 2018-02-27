setClass(
  "pc_set",
  slots = list(pc = "integer")
)

#' @export
setClass(
  "chord",
  slots = list(
    bass_pc = "integer",
    non_bass_pc_set = "integer"
  )
)

#' @export
setClass(
  "harmony_corpus",
  slots = list(
    compositions = "list"
  )
)

#' @export
setClass(
  "harmony_composition",
  slots = list(
    events = "integer"
  )
)

#' @export
setClass("pc_set_normal_form",
         slots = list(pc = "integer",
                      transposition = "integer"))

#' @export
setClass("pc_set_normal_order",
         slots = list(pc = "integer"))

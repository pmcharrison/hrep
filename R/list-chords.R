#' List chords
#'
#' Lists all chords of a given type.
#' @param type (Character scalar) Type to list.
#' @return A list of all chords of the requested type,
#' where the ith element corresponds to the ith element of the alphabet.
#' @export
list_chords <- function(type) {
  checkmate::qassert(type, "S1")
  tryCatch(n <- alphabet_size(type),
           error = function(e) stop("alphabet not defined for this type"))
  if (type == "pc_chord") {
    hrep::pc_chord_alphabet$by_id
  } else if (type == "pc_set") {
    hrep::pc_set_alphabet$by_id
  } else if (type == "pc_set_type") {
    hrep::pc_set_type_alphabet$by_id
  } else if (type == "pc_chord_type") {
    hrep::pc_chord_type_alphabet$by_id
  } else {
    seq_len(n) %>%
      coded_vec(type) %>%
      decode() %>%
      as.list()
  }
}

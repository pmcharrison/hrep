#' @export
alphabet_size <- function(type) {
  checkmate::qassert(type, "S1")
  if (type == "pc_chord")
    pc_chord_alphabet_size() else if (type == "pc_set")
      pc_set_alphabet_size() else if (type == "pc_chord_type")
        pc_chord_type_alphabet_size() else
          stop("undefined alphabet size for type = ", type)
}

#' @export
pc_set_alphabet_size <- function() {
  length(hrep::pc_set_alphabet$by_id)
}

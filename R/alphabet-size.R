#' Alphabet size
#'
#' Returns the alphabet size for a given type.
#' @param type (Character scalar) Type.
#' @return Alphabet size, as an integer scalar.
#' @export
alphabet_size <- function(type) {
  checkmate::qassert(type, "S1")
  if (type == "pc_chord")
    pc_chord_alphabet_size() else if (type == "pc_set")
      pc_set_alphabet_size() else if (type == "pc_chord_type")
        pc_chord_type_alphabet_size() else
          stop("undefined alphabet size for type = ", type)
}

pc_chord_alphabet_size <- function() {
  length(hrep::pc_chord_alphabet$by_id)
}

pc_set_alphabet_size <- function() {
  length(hrep::pc_set_alphabet$by_id)
}

pc_chord_type_alphabet_size <- function() {
  pc_chord_alphabet_size() / 12L
}

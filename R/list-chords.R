#' List chords
#'
#' Lists all chords of a given type.
#' @param type (Character scalar) Type to list.
#' @return A list of all chords of the requested type,
#' where the ith element corresponds to the ith element of the alphabet.
#' @export
list_chords <- function(type) {
  tryCatch(n <- alphabet_size(type),
           error = function(e) stop("alphabet not defined for this type"))
  seq_len(n) %>%
    coded_vec(type) %>%
    decode() %>%
    as.list()
}

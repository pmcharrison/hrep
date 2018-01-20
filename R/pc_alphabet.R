#' Get pitch class set alphabet
#'
#' Returns a list of all possible pitch class sets.
#' @return List of all possible pitch class sets.
#' @export
get_pc_set_alphabet <- function() {
  args <- list()
  for (i in 0:11) {
    args[[as.character(i)]] <- c(FALSE, TRUE)
  }
  spec <- do.call(expand.grid, args)
  res <- list()
  n <- nrow(spec)
  for (i in seq_len(n)) {
    pc_set <- (0:11)[which(as.logical(spec[i, ]))]
    if (length(pc_set) > 0) {
      res <- c(res, list(pc_set))
    }
  }
  res
}

#' @export
get_pc_set_alphabet_from_datasets <- function(
  datasets = list(
    HarmonyCorpora::classical,
    HarmonyCorpora::popular,
    HarmonyCorpora::jazz
  ), encode = FALSE
) {
  datasets %>%
    (function(x) do.call(c, x)) %>%
    get_pc_set_alphabet_from_dataset(encode = encode)
}

#' @export
get_pc_set_alphabet_from_dataset <- function(
  dataset, encode = FALSE
) {
  if (encode) {
    stop("Encoding not yet supported for pitch-class sets")
  }
  get_chord_alphabet_from_dataset(dataset) %>%
    decode_chords %>%
    lapply(HarmonyUtils::convert_pitch_to_pc_set) %>%
    unique %>%
    (function(x) x[order(vapply(x, function(y) {
      paste(y, collapse = " ")
    }, character(1)))])
}

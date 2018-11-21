#' @export
get_chord_counts <- function(x) UseMethod("get_chord_counts")
#' @export
get_chord_counts.harmony_corpus <- function(x) {
  stop("needs refactoring")
  events <- x %>% as.list %>% lapply(as.integer) %>% unlist(recursive = FALSE)
  table(events, dnn = NULL)
}

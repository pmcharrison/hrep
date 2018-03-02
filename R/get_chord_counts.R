#' @export
setGeneric("get_chord_counts", function(x) standardGeneric("get_chord_counts"))
setMethod("get_chord_counts", signature(x = "harmony_corpus"),
          function(x) {
            events <- x %>% as.list %>% lapply(as.integer) %>%
              unlist(recursive = FALSE)
            table(events, dnn = NULL)
          })

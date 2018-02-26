#' @export
setGeneric("get_chord_counts", function(x) standardGeneric("get_chord_counts"))
setMethod("get_chord_counts", signature(x = "harmony_corpus"),
          function(x) {
            events <- x %>% as.list %>% lapply(as.integer) %>%
              (function(y) do.call(c, y))
            table(events, dnn = NULL)
          })

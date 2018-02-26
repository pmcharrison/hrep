#' @export
setMethod(
  "as.integer", signature(x = "pc_set_normal_order"),
  function(x, ...) x@pc
)

#' @export
setMethod(
  "as.numeric", signature(x = "pc_set_normal_order"),
  function(x, ...) as.numeric(as.integer(x))
)

#' @export
setMethod(
  "show", signature(object = "pc_set_normal_order"),
  function(object) {
    cat("Pitch-class set (normal order): ",
        paste0("[", paste(as.integer(object), collapse = ", "), "]"),
        sep = "")
  }
)

#' @export
setGeneric("get_pc_set_normal_order", function(x) standardGeneric("get_pc_set_normal_order"),
           valueClass = "pc_set_normal_order")
setMethod(
  "get_pc_set_normal_order", signature(x = "numeric"),
  function(x) get_pc_set_normal_order(make_pc_set(x))
)
setMethod(
  "get_pc_set_normal_order", signature(x = "pc_set"),
  function(x) {
    x_int <- as.integer(x)
    res <- if (length(x_int) == 0) {
      integer()
    } else {
      n <- length(x_int)
      cycles <- cycle_vector(x_int)
      dist <- apply(cycles, 2, function(y) {
        get_ascending_pc_dist(cycles[, 1], y)
      })
      best <- seq_len(n)
      # First look at the distance between first and last PCs,
      # then between first and second-last, etc...
      for (i in seq(from = n, to = 1L)) {
        # Keep candidates that are the best so far
        best <- intersect(best,
                          which(dist[, i] == min(dist[, i])))
        if (length(best) == 1) break
      }
      if (length(best) == 0) stop("No normal orders found!")
      cycles[best[1], ]
    }
    new("pc_set_normal_order", pc = res)
  })

cycle_vector <- function(x) {
  as.matrix(
    if (length(x) == 0) x else {
      sapply(seq(from = 0L, length = length(x)),
             function(i) {
               x[1L + (seq(from = i, length.out = length(x)) %% length(x))]
             }
      )
    }
  )
}

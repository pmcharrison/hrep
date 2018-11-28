#' @export
print.pc_set_norm_order <- function(x, ...) {
  cat("Pitch-class set (normal order): ",
      paste0(paste(as.numeric(x), collapse = " "), "\n"),
      sep = "")
}

#' @export
c.pc_set_norm_order <- function(...) {
  x <- lapply(list(...), pc_set)
  x <- do.call(c, x)
}

#' @export
pc_set_norm_order <- function(x) UseMethod("pc_set_norm_order")

#' @export
pc_set_norm_order.numeric <- function(x) {
  pc_set_norm_order(pc_set(unclass(x)))
}

#' @export
pc_set_norm_order.pc_set <- function(x) {
  if (identical(length(x), 0L)) return(x)
  x <- as.numeric(x)
  n <- length(x)
  cycles <- cycle_vector(x)
  dist <- as.matrix(apply(cycles, 2, function(y) {
    ascending_pc_dist(cycles[, 1], y)
  }))
  best <- seq_len(n)
  # First look at the distance between first and last PCs,
  # then between first and second-last, etc...
  for (i in seq(from = n, to = 1L)) {
    # Keep candidates that are the best so far
    best <- intersect(best,
                      which(dist[, i] == min(dist[best, i])))
    if (length(best) == 1) break
  }
  if (length(best) == 0) stop("No normal orders found!")
  res <- cycles[best[1], ]
  class(res) <- c("pc_set_norm_order", "numeric")
  res
}

cycle_vector <- function(x) {
  as.matrix(
    if (length(x) == 0) x else {
      sapply(seq(from = 0L, length = length(x)),
             function(i) {
               x[1L + (seq(from = i, length.out = length(x)) %% length(x))]
             })})}

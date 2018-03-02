#' @export
print.pc_set_normal_order <- function(x, ...) {
  cat("Pitch-class set (normal order): ",
      paste0("[", paste(as.integer(x), collapse = ", "), "]\n"),
      sep = "")
}

get_pc_set_normal_order <- function(x) UseMethod("get_pc_set_normal_order")
get_pc_set_normal_order.pc_set < function(x) {
  get_pc_set_normal_order(as.integer(x))
}
get_pc_set_normal_order.numeric <- function(x) {
  get_pc_set_normal_order(as.integer(x))
}
get_pc_set_normal_order.integer <- function(x) {
  stopifnot(!anyDuplicated(x))
  stopifnot(all(x >= 0L & x < 12L))
  if (identical(length(x), 0L)) {
    return(new_pc_set(x))
  }
  n <- length(x)
  cycles <- cycle_vector(x)
  dist <- as.matrix(apply(cycles, 2, function(y) {
    get_ascending_pc_dist(cycles[, 1], y)
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
  class(res) <- "pc_set_normal_order"
  res
}

cycle_vector <- function(x) {
  as.matrix(
    if (length(x) == 0) x else {
      sapply(seq(from = 0L, length = length(x)),
             function(i) {
               x[1L + (seq(from = i, length.out = length(x)) %% length(x))]
             })})}

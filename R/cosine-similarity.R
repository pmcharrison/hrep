#' Cosine similarity
#'
#' Computes the cosine similarity between two numeric vectors.
#' @param x Numeric vector 1.
#' @param y Numeric vector 2.
#' @return Cosine similarity, as a numeric scalar.
#' @export
cosine_similarity <- function(x, y) {
  numerator <- sum(x * y)
  denominator <-
    sqrt(sum(x ^ 2)) *
    sqrt(sum(y ^ 2))
  numerator / denominator
}

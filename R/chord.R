#' Is chord
#'
#' Checks if an object is of class "chord".
#'
#' @param x Object to check.
#' @return Logical scalar.
#'
#' @export
is.chord <- function(x) {
  is(x, "chord")
}

#' @export
c.chord <- function(...) {
  stop("c() function is undefined for chords, for safety reasons...")
}

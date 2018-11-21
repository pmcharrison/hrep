# pc_set ####

#' @export
transpose <- function(x, interval) UseMethod("transpose")

check_is_valid_interval <- function(interval) {
  stopifnot(identical(length(interval), 1L))
  stopifnot(is.numeric(interval))
  stopifnot(all.equal(interval, round(interval)))
}

#' @export
transpose.pc_set <- function(x, interval) {
  check_is_valid_interval(interval)
  x_int <- as.integer(x)
  interval <- as.integer(interval)
  pc_set(sort((x_int + interval) %% 12L))
}

#' @export
transpose.pc_chord <- function(x, interval) {
  check_is_valid_interval(interval)
  interval <- as.integer(interval)
  x <- (x + interval) %% 12L
  if (length(x) > 1) {
    x[- 1] <- sort(x[- 1])
  }
  x
}

#' #' @export
#' `+.pc_set` <- function(x, y) {
#'   transpose(x, y)
#' }
#'
#' #' @export
#' `+.pc_chord` <- function(x, y) {
#'   transpose(x, y)
#' }

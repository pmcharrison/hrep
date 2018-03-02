# pc_set ####

#' @export
transpose <- function(x, interval, safe = TRUE) UseMethod(x)

#' @export
transpose.pc_set <- function(x, interval, safe = TRUE) {
  if (safe) {
    assertthat::assert_that(
      assertthat::is.scalar(interval),
      is.numeric(interval),
      interval == round(interval)
    )
    interval <- as.integer(interval)
  }
  (x + interval) %% 12L
}

# chord ####
setMethod(
  "transpose", signature(x = "chord"),
  function(x, interval) {
    assertthat::assert_that(
      assertthat::is.scalar(interval),
      is.numeric(interval),
      interval == round(interval)
    )
    interval <- as.integer(interval)
    x@bass_pc <- (x@bass_pc + interval) %% 12L
    x@non_bass_pc_set <- sort((x@non_bass_pc_set + interval) %% 12L)
    x
  })
setMethod(
  "+", signature(e1 = "chord", e2 = "numeric"),
  function(e1, e2) transpose(x = e1, interval = e2)
)
setMethod(
  "+", signature(e1 = "numeric", e2 = "chord"),
  function(e1, e2) transpose(interval = e1, x = e2)
)
setMethod(
  "-", signature(e1 = "chord", e2 = "numeric"),
  function(e1, e2) transpose(x = e1, interval = - e2)
)

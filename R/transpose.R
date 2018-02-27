# pc_set ####
setMethod(
  "transpose", signature(x = "pc_set"),
  function(x, interval) {
    assertthat::assert_that(
      assertthat::is.scalar(interval),
      is.numeric(interval),
      interval == round(interval)
    )
    interval <- as.integer(interval)
    x@pc <- sort((x@pc + interval) %% 12L)
    x
  }
)
setMethod(
  "+", signature(e1 = "pc_set", e2 = "numeric"),
  function(e1, e2) transpose(x = e1, interval = e2)
)
setMethod(
  "+", signature(e1 = "numeric", e2 = "pc_set"),
  function(e1, e2) transpose(interval = e1, x = e2)
)
setMethod(
  "-", signature(e1 = "pc_set", e2 = "numeric"),
  function(e1, e2) transpose(x = e1, interval = - e2)
)

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

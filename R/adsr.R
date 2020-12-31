#' ADSR
#'
#' Filters an object through the Attack-Decay-Sustain-Release (ADSR) envelope model,
#' implemented with linear interpolation.
#'
#' @param x
#' Object to filter; will be coerced to a wave object via \code{\link{wave}}.
#'
#' @param attack
#' (Positive numeric scalar)
#' Duration of the attack portion, in seconds.
#' At the end of the attack portion, the envelope amplitude reaches 1.
#'
#' @param decay
#' (Positive numeric scalar)
#' Duration of the decay portion, in seconds.
#'
#' @param sustain
#' (Numeric scalar between 0 and 1)
#' Envelope amplitude during the sustain portion.
#'
#'
#' @param hold
#' (Positive numeric scalar)
#' Duration of the sustain portion, in seconds.
#'
#' @param release
#' (Positive numeric scalar)
#' Duration of the release portion, in seconds.
#'
#' @rdname add_adsr
add_adsr <- function(
  x,
  attack,
  decay,
  sustain,
  hold,
  release,
  ...
) {
  checkmate::qassert(attack, "N1[0,)")
  checkmate::qassert(decay, "N1[0,)")
  checkmate::qassert(sustain, "N1[0,1])")
  checkmate::qassert(hold, "N1[0,)")
  checkmate::qassert(release, "N1[0,)")
  UseMethod("add_adsr")
}

add_adsr.default <- function(
  x,
  attack,
  decay,
  sustain,
  hold,
  release,
  ...
) {
  add_adsr.wave(
    wave(x, ...),
    attack = attack,
    decay = decay,
    sustain = sustain,
    hold = hold,
    release = release
  )
}

#' @rdname add_adsr
add_adsr.wave <- function(
  x,
  attack,
  decay,
  sustain,
  hold,
  release,
  ...
) {
  anchors <- tibble::tribble(
    ~ time,                          ~ amplitude,
    0,                               0,
    attack,                          1,
    attack + decay,                  sustain,
    attack + decay + hold,           sustain,
    attack + decay + hold + release, 0
  )
  anchors$sample <- 1 + round(anchors$time * sample_rate(x))
  envelope <- approx(x = anchors$sample,
                     y = anchors$amplitude,
                     xout = seq_along(x),
                     rule = 2)$y
  x * envelope
}

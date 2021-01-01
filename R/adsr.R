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
#' @inheritDotParams wave
#'
#' @rdname adsr_filter
#' @export
adsr_filter <- function(
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
  checkmate::qassert(sustain, "N1[0,1]")
  checkmate::qassert(hold, "N1[0,)")
  checkmate::qassert(release, "N1[0,)")
  UseMethod("adsr_filter")
}

#' @export
adsr_filter.default <- function(
  x,
  attack,
  decay,
  sustain,
  hold,
  release,
  ...
) {
  adsr_filter.wave(
    wave(x, ...),
    attack = attack,
    decay = decay,
    sustain = sustain,
    hold = hold,
    release = release
  )
}

#' @rdname adsr_filter
#' @export
adsr_filter.wave <- function(
  x,
  attack,
  decay,
  sustain,
  hold,
  release,
  ...
) {
  if (attack == 0) attack <- 1e-10
  if (decay == 0) decay <- 1e-10
  if (hold == 0) hold <- 1e-10
  if (release == 0) release <- 1e-10

  anchors <- tibble::tribble(
    ~ time,                          ~ amplitude,
    0,                               0,
    attack,                          1,
    attack + decay,                  sustain,
    attack + decay + hold,           sustain,
    attack + decay + hold + release, 0
  )
  anchors$sample <- 1 + anchors$time * sample_rate(x)
  envelope <- stats::approx(x = anchors$sample,
                            y = anchors$amplitude,
                            xout = seq_along(x),
                            rule = 2)$y
  x * envelope
}

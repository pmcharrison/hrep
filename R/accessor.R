#' Accessors
#'
#' These functions allow access to certain object properties.
#' @param x Object to access.
#' @param value New property value.
#' @md
#' @details
#' * \code{x_unit}: the units of the x axis
#' * \code{y_unit}: the units of the y axis
#' * \code{lower}: the lower bound of a spectrum
#' * \code{upper}: the upper bound of a spectrum
#' * \code{low_eq}: whether the lower bound is an equality bound (i.e. x >= y)
#' * \code{high_eq}: whether the upper bound is an equality bound (i.e. x <= y)
#' * \code{label}: object's label
#' * \code{x_lab}: label for the x axis
#' * \code{y_lab}: label for the y axis
#' * \code{freq}: numeric vector of frequencies
#' * \code{pitch}: numeric vector of pitches
#' * \code{amp}: numeric vector of amplitudes
#' * \code{transposition}: object's transposition
#' @rdname access
#' @name access
NULL

#' @rdname access
#' @export
x_unit <- function(x) UseMethod("x_unit")

#' @rdname access
#' @export
y_unit <- function(x) UseMethod("y_unit")

`y_unit<-` <- function(x, value) UseMethod("y_unit<-")

#' @rdname access
#' @export
lower <- function(x) UseMethod("lower")

#' @rdname access
#' @export
upper <- function(x) UseMethod("upper")

#' @rdname access
#' @export
low_eq <- function(x) UseMethod("low_eq")

#' @rdname access
#' @export
high_eq <- function(x) UseMethod("high_eq")

#' @rdname access
#' @export
label <- function(x) UseMethod("label")

#' @rdname access
#' @export
x_lab <- function(x) UseMethod("x_lab")

#' @rdname access
#' @export
y_lab <- function(x) UseMethod("y_lab")

`y_lab<-` <- function(x, value) UseMethod("y_lab<-")

#' @rdname access
#' @export
freq <- function(x) UseMethod("freq")

#' @rdname access
#' @export
`freq<-` <- function(x, value) UseMethod("freq<-")

#' @rdname access
#' @export
pitch <- function(x) UseMethod("pitch")

#' @rdname access
#' @export
`pitch<-` <- function(x, value) UseMethod("pitch<-")

#' @rdname access
#' @export
amp <- function(x) UseMethod("amp")

#' @rdname access
#' @export
`amp<-` <- function(x, value) UseMethod("amp<-")

#' @export
transposition <- function(x) UseMethod("transposition")

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessor.R
\name{access}
\alias{access}
\alias{x_unit}
\alias{y_unit}
\alias{lower}
\alias{upper}
\alias{low_eq}
\alias{high_eq}
\alias{label}
\alias{x_lab}
\alias{y_lab}
\alias{freq}
\alias{freq<-}
\alias{pitch}
\alias{pitch<-}
\alias{pc}
\alias{pc<-}
\alias{amp}
\alias{amp<-}
\alias{transposition}
\alias{sample_rate}
\title{Accessors}
\usage{
x_unit(x)

y_unit(x)

lower(x)

upper(x)

low_eq(x)

high_eq(x)

label(x)

x_lab(x)

y_lab(x)

freq(x)

freq(x) <- value

pitch(x)

pitch(x) <- value

pc(x)

pc(x) <- value

amp(x)

amp(x) <- value

transposition(x)

sample_rate(x)
}
\arguments{
\item{x}{Object to access.}

\item{value}{New property value.}
}
\description{
These functions allow access to certain object properties.
}
\details{
\itemize{
\item \code{x_unit}: the units of the x axis
\item \code{y_unit}: the units of the y axis
\item \code{lower}: the lower bound of a spectrum
\item \code{upper}: the upper bound of a spectrum
\item \code{low_eq}: whether the lower bound is an equality bound (i.e. x >= y)
\item \code{high_eq}: whether the upper bound is an equality bound (i.e. x <= y)
\item \code{label}: object's label
\item \code{x_lab}: label for the x axis
\item \code{y_lab}: label for the y axis
\item \code{freq}: numeric vector of frequencies
\item \code{pitch}: numeric vector of pitches
\item \code{amp}: numeric vector of amplitudes
\item \code{transposition}: object's transposition
\item \code{sample_rate}: object's sample rate
}
}

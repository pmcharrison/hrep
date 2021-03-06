% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fr-chord.R
\name{fr_chord}
\alias{fr_chord}
\alias{fr_chord.numeric}
\alias{fr_chord.character}
\alias{fr_chord.pi_chord}
\alias{fr_chord.pc_set}
\alias{fr_chord.pc_chord}
\alias{fr_chord.fr_chord}
\alias{is.fr_chord}
\title{Frequency chord}
\usage{
fr_chord(x)

\method{fr_chord}{numeric}(x)

\method{fr_chord}{character}(x)

\method{fr_chord}{pi_chord}(x)

\method{fr_chord}{pc_set}(x)

\method{fr_chord}{pc_chord}(x)

\method{fr_chord}{fr_chord}(x)

is.fr_chord(x)
}
\arguments{
\item{x}{Object to represent as a frequency chord.}
}
\value{
Returns an object of class \code{fr_chord}.
}
\description{
This function represents an object as a frequency chord.
A frequency chord is defined as a set of non-duplicated
frequencies, expressed in Hz.
}

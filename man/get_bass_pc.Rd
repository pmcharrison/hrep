% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pc-chord.R
\name{get_bass_pc}
\alias{get_bass_pc}
\alias{get_bass_pc.default}
\alias{get_bass_pc.pc_chord}
\alias{get_non_bass_pc.default}
\alias{get_non_bass_pc.pc_chord}
\title{Get bass pitch class}
\usage{
get_bass_pc(x)

\method{get_bass_pc}{default}(x)

\method{get_bass_pc}{pc_chord}(x)

\method{get_non_bass_pc}{default}(x)

\method{get_non_bass_pc}{pc_chord}(x)
}
\arguments{
\item{x}{Object to analyse.}
}
\value{
The bass pitch class, as a numeric scalar.
}
\description{
Gets the bass pitch class of a sonority.
}

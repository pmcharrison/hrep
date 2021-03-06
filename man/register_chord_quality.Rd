% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chord-qualities.R
\name{register_chord_quality}
\alias{register_chord_quality}
\title{Register chord quality}
\usage{
register_chord_quality(key, value, overwrite = FALSE)
}
\arguments{
\item{key}{(Character scalar) Textual key identifying the chord quality.}

\item{value}{(Integer vector) Chord quality,
provided as an integer vector of non-duplicated pitch classes
in ascending order.}

\item{overwrite}{(Logical scalar) If \code{FALSE}, an error will
be thrown when trying to redefine a pre-existing key.}
}
\description{
This function registers a new chord quality in the database
used by the \code{\link{decode_chord_quality}} function.
Unless overwritten,
this chord quality will persist for the remainder of the R session.
}
\details{
If you have some chord qualities to add to the \code{hrep} package,
please register an issue or submit a pull request at
\url{https://github.com/pmcharrison/hrep}.
}
\examples{
decode_chord_quality("my_chord") # returns NULL
register_chord_quality("my_chord", c(0, 1, 2))
decode_chord_quality("my_chord") # returns c(0, 1, 2)

}
\seealso{
\itemize{
\item \code{\link{decode_chord_quality}} for accessing the database.
\item \code{\link{initialise_chord_qualities}} for resetting the database.
}
}

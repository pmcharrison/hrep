% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pc-set.R
\name{map_pc_chord_id_to_pc_set_id}
\alias{map_pc_chord_id_to_pc_set_id}
\title{Map pitch-class chords to pitch-class sets}
\usage{
map_pc_chord_id_to_pc_set_id(pc_chord_id)
}
\arguments{
\item{pc_chord_id}{Numeric vector of pitch-class chord codes.}
}
\value{
Numeric vector of pitch-class set codes.
}
\description{
This vectorised function maps encoded pitch-class chords ("pc_chord")
(as produced by \code{\link{encode}})
to encoded pitch-class sets ("pc_set").
}

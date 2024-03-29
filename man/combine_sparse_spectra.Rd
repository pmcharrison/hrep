% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse-spectrum.R
\name{combine_sparse_spectra}
\alias{combine_sparse_spectra}
\title{Combine sparse spectra}
\usage{
combine_sparse_spectra(..., digits = 6, coherent = FALSE)
}
\arguments{
\item{...}{Sparse spectra to combine
(see \code{\link{sparse_pi_spectrum}} and \code{\link{sparse_pc_spectrum}}).}

\item{digits}{(Integerish scalar)
The MIDI pitch(-class) of each partial will be rounded to this number
of digits.}

\item{coherent}{Whether the amplitudes from different spectral components should be combined
assuming coherent summation, where the amplitudes simply add together
(default is \code{FALSE}).
Otherwise incoherent summation is used, where the amplitudes are squared, added, then
square rooted.}
}
\value{
A sparse spectrum object.
}
\description{
This function combines a series of sparse spectra into one spectrum
assuming incoherent amplitude summation.
This involves a rounding process,
by which the MIDI pitch(-class) of each partial
is rounded to a specified number of digits.
}

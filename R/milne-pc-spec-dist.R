#' Milne's pitch-class spectral distance
#'
#' Computes Milne's pitch-class spectral distance measure between two sonorities.
#'
#' @param x First sonority, passed to \code{\link{milne_pc_spectrum}}.
#' @param y Second sonority, passed to \code{\link{milne_pc_spectrum}}.
#' @param ... Further arguments passed to \code{\link{milne_pc_spectrum}}.
#'
#' @return
#' A numeric scalar where greater values indicate greater distance.
#' 0 is the minimum possible distance.
#' 1 corresponds to two uncorrelated spectra;
#' values greater than 1 are possible if the spectra are inversely correlated.
#'
#' @details
#' See \insertCite{Milne2016;textual}{hrep} for details.
#'
#' @export
milne_pc_spec_dist <- function(x, y, ...) {
  1 - cosine_similarity(
    milne_pc_spectrum(x, ...),
    milne_pc_spectrum(y, ...)
  )
}

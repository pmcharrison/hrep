#' Pitch-class chord alphabet
#'
#' This dataset defines an alphabet for the type "pc_chord".
#' It is a list with two components,
#' defining a forward and backward
#' mapping between integers and "pc_chord" objects:
#' * \code{by_id}:
#' A list object.
#' The ith element of this list contains the pitch-class chord
#' corresponding to integer i.
#' * \code{by_pc_chord}:
#' An environment object.
#' A pitch-class chord can be mapped to an integer by
#' taking the pitch-class chord object,
#' converting it to a string representation (with \code{as.character}),
#' and accessing the object within this environment with this name.
#' The result will be a coded vector (see \code{\link{coded_vec}})
#' of length 1.
#' @md
#' @name pc_chord_alphabet
#' @docType data
#' @keywords data
NULL

#' Pitch-class set alphabet
#'
#' This dataset defines an alphabet for the type "pc_set".
#' It is a list with two components,
#' defining a forward and backward
#' mapping between integers and "pc_set" objects:
#' * \code{by_id}:
#' A list object.
#' The ith element of this list contains the pitch-class set
#' corresponding to integer i.
#' * \code{by_pc_set}:
#' An environment object.
#' A pitch-class set can be mapped to an integer by
#' taking the pitch-class set object,
#' converting it to a string representation (with \code{as.character}),
#' and accessing the object within this environment with this name.
#' The result will be a coded vector (see \code{\link{coded_vec}})
#' of length 1.
#' @md
#' @name pc_set_alphabet
#' @docType data
#' @keywords data
NULL

#' pc_chord to pc_set map
#'
#' This integer vector provides the mapping between
#' pitch-class chords ("pc_chord" objects) and pitch-class sets ("pc_set" objects).
#' This can help computational efficiency in some situations.
#'
#' If \code{i} is the integer corresponding to an encoded pitch-class chord,
#' then the corresponding encoded pitch-class set is
#' equal to the ith element of this vector.
#' @name pc_chord_id_to_pc_set_id_map
#' @docType data
#' @keywords data
#' @examples
#' i <- encode(pc_chord(c(4, 0, 7)))
#' j <- pc_chord_id_to_pc_set_id_map[i]
#' decode(coded_vec(j, "pc_set"))[[1]]
NULL

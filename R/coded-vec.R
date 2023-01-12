#' Coded vector
#'
#' This function creates a coded vector.
#' Coded vectors are used to represent sequences of symbols
#' from a finite alphabet.
#' Each symbol should be coded as an integer using a bijective mapping.
#' @param x (Integer vector) The coded sequence.
#' @param type (Character scalar) Identifies the symbol \code{\link{type}}.
#' @param metadata (List) A (possibly-empty) list of metadata information.
#' @return An object of class "coded_vec".
#' @seealso \code{\link{vec}}.
#' @export
coded_vec <- function(x, type, metadata = list()) {
  checkmate::qassert(x, "X")
  checkmate::qassert(type, "S1")
  checkmate::qassert(metadata, "l")
  x <- as.integer(x)
  class(x) <- c(paste0("coded_vec_", type), "coded_vec", "integer")
  type(x) <- type
  metadata(x) <- metadata
  x
}

#' @export
type.coded_vec <- function(x) {
  attr(x, "type")
}

`type<-.coded_vec` <- function(x, value) {
  attr(x, "type") <- value
  x
}

#' @export
metadata.coded_vec <- function(x) {
  attr(x, "metadata")
}

#' @export
`metadata<-.coded_vec` <- function(x, value) {
  attr(x, "metadata") <- value
  x
}

#' Type-checking for "coded_vec"
#'
#' Checks whether an object is of type "coded_vec".
#' @param x Object to check.
#' @return Logical scalar.
#' @export
is.coded_vec <- function(x) {
  is(x, "coded_vec")
}



#' @export
print.coded_vec <- function(x, ...) {
  cat("Encoded vector of type '", type(x),
      "', length = ", num_elements(x),
      if (length(metadata(x)) > 0L) " (metadata available)", "\n", sep = "")
}

#' @rdname num_elements
#' @export
num_elements.coded_vec <- function(x) length(x)

#' Encode
#'
#' Transforms a given object into an integer-based encoding.
#'
#' @param x Object to encode, as created by
#' \code{\link{pc_set}}, \code{\link{pc_set_type}}, \code{\link{pc_chord}},
#' or \code{\link{pc_chord_type}}.
#'
#' @return Encoded object.
#'
#' @details
#' Encoding is currently defined for the following types:
#' * \code{\link{pc_set}}
#' * \code{\link{pc_set_type}}
#' * \code{\link{pc_chord}}
#' * \code{\link{pc_chord_type}}
#'
#' An error will be thrown when trying to encode objects of other types.
#'
#' The \code{encode} function is vectorised and uses precomputed encodings.
#' The following functions represent alternative implementations
#' that are not vectorised and
#' do not rely on precomputed encodings.
#' Note that these alternative implementations do not necessarily
#' perform systematic input checking.
#' - \code{encode_pc_set}
#' - \code{encode_pc_chord_type}
#' - \code{encode_pc_chord}
#'
#' @md
#' @seealso \code{\link{decode}} for the reverse operation.
#' @export
encode <- function(x) {
  UseMethod("encode")
}

#' @rdname encode
#' @export
encode_pc_set <- function(x) {
  as.integer(sum((2L ^ (11:0)) * (0:11 %in% x)))
}

# Order-insensitive, little error checking
#' @rdname encode
#' @export
encode_pc_chord_type <- function(x) {
  if (length(x) == 0) stop("invalid pc_chord_type")
  as.integer(1L + sum((2L ^ (10:0)) * (1:11 %in% x)))
}

#' @rdname encode
#' @export
encode_pc_chord <- function(x) {
  bass <- x[1]
  chord_type <- (x - bass) %% 12L
  as.integer(2048L * bass + encode_pc_chord_type(chord_type))
}

#' @rdname encode
#' @export
encode.vec <- function(x) {
  coded_vec(purrr::map_int(x, encode),
            type = type(x),
            metadata = metadata(x))
}

#' @rdname encode
#' @export
encode.coded_vec <- function(x) x

#' @rdname is.coded
#' @export
is.coded.coded_vec <- function(x) TRUE

#' Decode
#'
#' Decodes an object from an integer-based encoding.
#'
#' The \code{decode} function is vectorised and uses precomputed encodings.
#' The following functions represent alternative implementations
#' that are not vectorised and do not rely on precomputed encodings:
#' - \code{decode_pc_set}
#' - \code{decode_pc_chord_type}
#' - \code{decode_pc_chord}
#'
#' @return
#' \code{decode} returns an object of class \code{vec};
#' the other functions return integer vectors.
#'
#' @md
#'
#' @param x Object to decode.
#'
#' @param x_type
#' (Character scalar)
#' The type that \code{x} is intended to represent (e.g. \code{"pc_chord"}).
#'
#' @seealso \code{\link{encode}} for the reverse operation.
#'
#' @rdname decode
#' @export
decode <- function(x, x_type = type(x)) {
  checkmate::qassert(x, "X")
  f <- paste0("decode.coded_vec_", x_type)
  vec(do.call(f, args = list(x)),
      type = x_type,
      metadata = metadata(x))
}

#' @rdname decode
#' @export
decode_pc_set <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || x < 1 || x > 4095)
    stop("invalid pc_set id")
  x <- as.integer(x)
  binary <- rev(intToBits(x)[1:12] == 1)
  .pc_set((0:11)[binary])
}

#' @rdname decode
#' @export
decode_pc_chord_type <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || x < 1 || x > 2048)
    stop("invalid pc_chord_type id")
  x <- as.integer(x)
  binary <- rev(intToBits(x - 1L)[1:11] == 1)
  .pc_chord_type(c(0L, (1:11)[binary]))
}

#' @rdname decode
#' @export
decode_pc_chord <- function(x) {
  x <- as.integer(x)
  bass <- (x - 1) %/% 2048L
  chord_type <- decode_pc_chord_type(((x - 1) %% 2048L) + 1)
  .pc_chord(bass_pc = bass,
            other_pc = sort.int((chord_type[-1L] + bass) %% 12L))
}

#' @rdname transform_symbols
#' @export
transform_symbols.coded_vec <- function(x, f, type, ...) {
  encode(transform_symbols(decode(x), f, type = type, ...))
}

#' @export
`[.coded_vec` <- function(x, i) {
  coded_vec(as.integer(x)[i], type = type(x), metadata = metadata(x))
}

#' @export
`[<-.coded_vec` <- function(x, i, value) {
  checkmate::qassert(value, "X")
  NextMethod("[<-.corpus")
}

#' @export
`[[<-.coded_vec` <- function(x, i, value) {
  checkmate::qassert(value, "X1")
  NextMethod("[<-.corpus")
}

#' @export
view.coded_vec <- function(x, ...) {
  view(decode(x), ...)
}

#' Get chord qualities
#'
#' Returns a \code{\link[tibble]{tibble}} of chord qualities and different
#' textual labels for these chord qualities.
#' In practice most users will access this information through the
#' function \code{\link{decode_chord_quality}}.
#'
#' The \code{tibble} has one row for each unique chord quality,
#' where a chord quality is defined as a set of pitch classes
#' expressed relative to the chord root.
#'
#' - The \code{pitch_classes} column provides the chord quality as an integer vector,
#' corresponding to a list of pitch classes expressed relative to the chord root.
#' - The \code{scale_degrees} column lists the scale degrees present in the
#' chord quality in a more traditional human-readable format.
#' - The \code{description} column provides a textual description for the
#' chord quality.
#' - The remaining columns are prefixed by \code{key_},
#' and provide textual abbreviations for the chord quality.
#' These \code{key_} elements define the legal inputs for the function
#' \code{\link{decode_chord_quality}}.
#'
#' The data is read from the package file 'inst/chord-qualities.csv'.
#' Additions can be proposed by GitHub: \url{https://github.com/pmcharrison/hrep}.
#'
#' @seealso
#' - \code{\link{decode_chord_quality}} for decoding chord qualities.
#' - \code{\link{register_chord_quality}} for registering additional chord qualities.
#'
#' @md
#'
#' @export
get_chord_qualities_df <- function() {
  file <- system.file("chord-qualities.csv", package = "hrep", mustWork = TRUE)

  fixed_cols <- c("pitch_classes", "scale_degrees", "description")

  n_cols <- readr::read_csv(file, n_max = 1, col_names = FALSE, col_types = readr::cols()) %>%
    as.character() %>%
    {
      stopifnot(identical(.[1:3], fixed_cols),
                all(.[-(1:3)] == "NA"))
      length(.)
    }

  col_names <- c(fixed_cols,
                 paste("key_", seq_len(n_cols - length(fixed_cols)), sep = ""))
  which_keys <- grepl("key_", col_names) %>% which()

  df <- readr::read_csv(file,
                        col_names = col_names,
                        col_types = readr::cols(.default = "c"),
                        skip = 1)

  stopifnot(
    !anyNA(df[[1]]),
    all(purrr::map_lgl(df, is.character))
  )

  df$pitch_classes <- strsplit(df$pitch_classes, " ") %>%
    purrr::map(as.integer)

  purrr::walk(df$pitch_classes, assert_legal_pc_set)

  attr(df, "which_keys") <- which_keys

  df
}

assert_legal_pc_set <- function(x) {
  if (anyDuplicated(x) || !checkmate::qtest(x, "X+[0,12)") ||
      !all(x == sort(x)))
    stop("invalid pitch-class set: ",
         paste(x, collapse = " "))
}

CHORD_QUALITIES <- new.env()

#' Register chord quality
#'
#' This function registers a new chord quality in the database
#' used by the \code{\link{decode_chord_quality}} function.
#' Unless overwritten,
#' this chord quality will persist for the remainder of the R session.
#'
#' @param key
#' (Character scalar) Textual key identifying the chord quality.
#'
#' @param value
#' (Integer vector) Chord quality,
#' provided as an integer vector of non-duplicated pitch classes
#' in ascending order.
#'
#' @param overwrite
#' (Logical scalar) If \code{FALSE}, an error will
#' be thrown when trying to redefine a pre-existing key.
#'
#' @examples
#' decode_chord_quality("my_chord") # returns NULL
#' register_chord_quality("my_chord", c(0, 1, 2))
#' decode_chord_quality("my_chord") # returns c(0, 1, 2)
#'
#' @seealso
#' - \code{\link{decode_chord_quality}} for accessing the database.
#' - \code{\link{initialise_chord_qualities}} for resetting the database.
#'
#' @details
#' If you have some chord qualities to add to the \code{hrep} package,
#' please register an issue or submit a pull request at
#' \url{https://github.com/pmcharrison/hrep}.
#'
#' @md
#'
#' @export
register_chord_quality <- function(key, value, overwrite = FALSE) {
  checkmate::qassert(key, "S1")
  checkmate::qassert(overwrite, "B1")
  assert_legal_pc_set(value)
  if (nchar(key) == 0) stop("the empty string is not a permissible chord label")
  if (!is.null(CHORD_QUALITIES[[key]]) && !overwrite)
    stop("cannot insert duplicate chord quality: ", key)
  CHORD_QUALITIES[[key]] <- as.integer(value)
}

#' Initialise chord qualities database
#'
#' (Re)initialises the chord qualities used by the \code{\link{decode_chord_quality}}
#' function to values specified by the \code{\link{get_chord_qualities_df}}
#' function.
#'
#' @export
initialise_chord_qualities <- function() {
  CHORD_QUALITIES <<- new.env()
  df <- get_chord_qualities_df()
  which_keys <- attr(df, "which_keys")
  for (i in seq_len(nrow(df))) {
    value <- df[[1]][[i]]
    keys <- df[i, which_keys] %>% as.character() %>% na.omit()
    purrr::walk(keys, register_chord_quality, value)
  }
}

#' Decode chord quality
#'
#' Decodes a given textual label into the implied chord quality.
#'
#' No preprocessing is applied to \code{x}.
#'
#' @param x
#' (Character scalar)
#' Label to decode.
#'
#' @param must_work
#' (Logical scalar)
#' If \code{TRUE}, the function will throw an error if the textual label
#' is not recognised.
#'
#' @param return
#' If the textual label is recognised,
#' returns an integer vector corresponding to the pitch-class set
#' expressed relative to the chord root,
#' otherwise returns \code{NULL}
#' (or an error if \code{must_work} is \code{TRUE}).
#'
#' @seealso
#' - \code{\link{register_chord_quality}} for registering additional chord qualities
#' in the database.
#' - \code{\link{initialise_chord_qualities}} for resetting the database.
#'
#' @md
#'
#' @export
decode_chord_quality <- function(x, must_work = FALSE) {
  checkmate::qassert(x, "S1")
  checkmate::qassert(must_work, "B1")
  if (nchar(x) == 0) stop("the empty string is not a permissible chord label")
  res <- CHORD_QUALITIES[[x]]
  if (must_work && is.null(res)) stop("could not decode token: ", x)
  res
}

initialise_chord_qualities()

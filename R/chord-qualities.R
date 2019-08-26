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
  if (anyDuplicated(x) || !checkmate::qtest(x, "X+[0,12)"))
    stop("invalid pitch-class set: ",
         paste(x, collapse = " "))
}

CHORD_QUALITIES <- new.env()

register_chord_quality <- function(key, value) {
  checkmate::qassert(key, "S1")
  assert_legal_pc_set(value)
  if (!is.null(CHORD_QUALITIES[[key]]))
    stop("cannot insert duplicate chord quality: ", key)
  CHORD_QUALITIES[[key]] <- value
}

initialise_chord_qualities <- function() {
  df <- get_chord_qualities_df()
  which_keys <- attr(df, "which_keys")
  for (i in seq_len(nrow(df))) {
    value <- df[[1]][[i]]
    keys <- df[i, which_keys] %>% as.character() %>% na.omit()
    purrr::walk(keys, register_chord_quality, value)
  }
}

decode_chord_quality <- function(x, must_work = FALSE) {
  checkmate::qassert(x, "S1")
  checkmate::qassert(must_work, "B1")
  res <- CHORD_QUALITIES[[x]]
  if (must_work && is.null(res)) stop("could not decode token: ", x)
  res
}

initialise_chord_qualities()

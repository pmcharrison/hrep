#' Note: this could be done more efficiently with ifft
#' @export
sparse_spectrum_to_waveform <- function(
  frequency,
  amplitude,
  seconds = 1,
  sample_rate = 44e3,
  bit = 16
) {
  x <- seq(from = 0, to = seconds, length.out = sample_rate * seconds)
  y <- mapply(
    function(freq, amplitude) {
      sin(2 * pi * freq * x) * amplitude
    },
    frequency,
    amplitude
  ) %>% rowSums %>%
    (function (y) y / max(abs(y))) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  data.frame(t = x, y = y)
}

reduce_by_key <- function(keys, values, f, key_type = "character") {
  stopifnot(
    length(keys) == length(values),
    is.function(f)
  )
  keys <- as.character(keys)
  env <- new.env()
  n <- length(keys)
  for (i in seq_len(n)) {
    key <- keys[i]
    value <- values[i]
    env[[key]] <- if (is.null(env[[key]])) {
      value
    } else {
      do.call(f, list(env[[key]], value))
    }
  }
  env_to_df(env, key_type = key_type)
}

env_to_df <- function(env, sort_by_key = TRUE, decreasing = FALSE, key_type = "character") {
  stopifnot(
    is.environment(env)
  )
  as.list(env) %>%
    (function(x) {
      data.frame(
        key = names(x) %>% as(key_type),
        value = unlist(x),
        stringsAsFactors = FALSE
      )
    }) %>%
    (function(df) {
      if (sort_by_key) {
        df[order(df$key, decreasing = decreasing), ]
      } else df
    }) %>% remove_row_names
}

remove_row_names <- function(df) {
  rownames(df) <- NULL
  df
}

#' @export
rename_columns <- function(df, replace, warn_missing = TRUE) {
  names(df) <- plyr::revalue(
    names(df), replace = replace, warn_missing = warn_missing
  )
  df
}

#' @export
add_attributes <- function(df, spec) {
  col_names <- names(spec)
  stopifnot(
    all(col_names %in% names(df))
  )
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    attributes <- spec[[i]]
    for (j in seq_along(attributes)) {
      attr_name <- names(attributes)[j]
      attr <- attributes[[j]]
      attr(df[[col_name]], attr_name) <- attr
    }
  }
  df
}

#' @export
rep_to_match <- function(x, y) {
  if (length(x) == 1) {
    rep(x, times = length(y))
  } else if (length(x) != length(y)) {
    stop("<x> must either have length 1 or the same length as <y>.")
  } else {
    x
  }
}

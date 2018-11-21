library(hutil)

#' Get pitch class set alphabet
#'
#' Returns a list of all possible pitch class sets.
#' @return List of all possible pitch class sets.
get_pc_set_alphabet <- function(format = "both") {
  stopifnot(
    format %in% c("by_id", "by_pc_set", "both")
  )
  args <- list()
  for (i in 0:11) {
    args[[as.character(i)]] <- c(FALSE, TRUE)
  }
  spec <- do.call(expand.grid, args)
  res <- list()
  n <- nrow(spec)
  for (i in seq_len(n)) {
    pc_set <- (0:11)[which(as.logical(spec[i, ]))]
    if (length(pc_set) > 0) {
      res <- c(res, list(pc_set(pc_set)))
    }
  }
  if (format == "by_id") {
    res
  } else {
    hash <- new.env()
    for (i in seq_along(res)) {
      key <- as.character(res[[i]])
      hash[[key]] <- i
    }
    if (format == "by_pc_set") {
      hash
    } else if (format == "both") {
      list(
        by_id = res,
        by_pc_set = hash
      )
    } else stop("Unrecognised <format>")
  }
}

pc_set_alphabet <- get_pc_set_alphabet("both")
usethis::use_data(pc_set_alphabet, overwrite = TRUE)

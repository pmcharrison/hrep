library(hrep)
library(magrittr)

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

list_pc_chords_with_bass_note <- function(bass_pc) {
  sets::set_power(x = setdiff(0:11, bass_pc)) %>%
    as.list %>%
    lapply(function(y) pc_chord(c(bass_pc, as.integer(y))))
}

get_pc_chord_alphabet <- function() {
  pc_chord_alphabet <- unlist(lapply(0:11, list_pc_chords_with_bass_note),
                              recursive = FALSE)
  pc_chord_ids <- seq_along(pc_chord_alphabet)
  map <- new.env(parent = emptyenv())
  for (pc_chord_id in pc_chord_ids) {
    pc_chord <- pc_chord_alphabet[[pc_chord_id]]
    key <- as.character(pc_chord)
    map[[key]] <- pc_chord_id
  }
  list(by_id = pc_chord_alphabet,
       by_pc_chord = map)
}

pc_chord_alphabet <- get_pc_chord_alphabet()
pc_set_alphabet <- get_pc_set_alphabet("both")
pc_chord_id_to_pc_set_id_map <- vapply(
  pc_chord_alphabet$by_id,
  function(pc_chord) {
    encode(pc_set(pc_chord))
  },
  integer(1)
)

usethis::use_data(pc_chord_alphabet, pc_set_alphabet,
                  pc_chord_id_to_pc_set_id_map,
                  overwrite = TRUE, internal = FALSE)

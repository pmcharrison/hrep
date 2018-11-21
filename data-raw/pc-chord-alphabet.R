library(hutil)
library(magrittr)

list_pc_chords_with_bass_note <- function(bass_pc) {
  sets::set_power(x = setdiff(0:11, bass_pc)) %>%
    as.list %>%
    lapply(function(y) pc_chord(bass_pc, as.integer(y)))
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
usethis::use_data(pc_chord_alphabet, overwrite = TRUE)

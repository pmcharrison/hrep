library(hutil)
pc_chord_id_to_pc_set_id_map <- vapply(
  pc_chord_alphabet$by_id,
  function(pc_chord) {
    encode(as.pc_set(pc_chord))
  },
  integer(1)
)
usethis::use_data(pc_chord_id_to_pc_set_id_map, overwrite = TRUE, internal = FALSE)

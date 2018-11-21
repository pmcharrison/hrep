chord_id_to_pc_set_id_map <- vapply(
  hutil::chord_alphabet$by_id,
  function(chord) {
    encode_pc_set(convert_pitch_to_pc_set(chord))
  },
  integer(1)
)
devtools::use_data(chord_id_to_pc_set_id_map, overwrite = TRUE)

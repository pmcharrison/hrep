pc_set_alphabet <- hrep:::precompute_pc_set_alphabet()
pc_chord_alphabet <- hrep:::precompute_pc_chord_alphabet()
pc_chord_type_alphabet <- hrep:::precompute_pc_chord_type_alphabet()
pc_set_type_alphabet <- hrep:::precompute_pc_set_type_alphabet()

pc_chord_id_to_pc_set_id_map <-
  hrep:::precompute_pc_chord_id_to_pc_set_id_map(pc_chord_alphabet)

usethis::use_data(
  pc_chord_alphabet,
  pc_chord_type_alphabet,
  pc_set_alphabet,
  pc_set_type_alphabet,
  pc_chord_id_to_pc_set_id_map,
  overwrite = TRUE, internal = FALSE
)

pc_set_alphabet <- hrep:::precompute_pc_set_alphabet()
pc_chord_alphabet <- hrep:::precompute_pc_chord_alphabet()
pc_chord_type_alphabet <- hrep:::precompute_pc_chord_type_alphabet()
pc_set_type_alphabet <- hrep:::precompute_pc_set_type_alphabet()

pc_chord_id_to_pc_set_id_map <-
  hrep:::precompute_pc_chord_id_to_pc_set_id_map(pc_chord_alphabet)

pc_chord_transpositions <-
  plyr::llply(
    1:11,
    function(transposition) {
      pc_chord_alphabet$by_id |>
        purrr::map(hrep::tp, transposition) |>
        purrr::map_int(hrep::encode)
    },
    .progress = "time"
  )

pc_set_transpositions <-
  plyr::llply(
    1:11,
    function(transposition) {
      pc_set_alphabet$by_id |>
        purrr::map(hrep::tp, transposition) |>
        purrr::map_int(hrep::encode)
    },
    .progress = "time"
  )


usethis::use_data(
  pc_chord_alphabet,
  pc_chord_type_alphabet,
  pc_set_alphabet,
  pc_set_type_alphabet,
  pc_chord_id_to_pc_set_id_map,
  pc_chord_transpositions,
  pc_set_transpositions,
  overwrite = TRUE, internal = FALSE
)

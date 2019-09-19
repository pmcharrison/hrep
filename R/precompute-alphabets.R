make_hash_table <- function(by_id, type) {
  hash <- new.env(parent = emptyenv())
  for (i in seq_along(by_id)) {
    key <- as.character(by_id[[i]])
    stopifnot(is.null(hash[[key]]))
    hash[[key]] <- coded_vec(i, type)
  }
  hash
}

precompute_alphabet <- function(alphabet_size,
                                decode_function,
                                type) {
  by_id <- plyr::llply(seq_len(alphabet_size), decode_function, .progress = "time")
  by_chord <- make_hash_table(by_id, type)
  list(by_id = by_id,
       by_chord = by_chord)
}

precompute_pc_set_type_alphabet <- function() {
  by_id_chr <- pc_set_type_alphabet <- 1:4095 %>%
    purrr::map(decode_pc_set) %>%
    purrr::map(pc_set_type) %>%
    purrr::map_chr(as.character) %>%
    factor(., levels = unique(.)) %>%
    levels()

  by_id <- by_id_chr %>%
    strsplit(split = " ") %>%
    purrr::map(as.integer) %>%
    purrr::map(.pc_set_type)

  list(by_id = by_id,
       by_chord = make_hash_table(by_id, "pc_set_type"))
}

precompute_pc_chord_id_to_pc_set_id_map <- function(pc_chord_alphabet) {
  pc_chord_alphabet$by_id %>%
    purrr::map(pc_set) %>%
    purrr::map_int(encode_pc_set)
}

precompute_pc_set_alphabet <- function() {
  precompute_alphabet(
    alphabet_size = 4095,
    decode_function = decode_pc_set,
    type = "pc_set"
  )
}

precompute_pc_chord_alphabet <- function() {
  precompute_alphabet(
    alphabet_size = 24576,
    decode_function = decode_pc_chord,
    type = "pc_chord"
  )
}

precompute_pc_chord_type_alphabet <- function() {
  precompute_alphabet(
    alphabet_size = 2048,
    decode_function = decode_pc_chord_type,
    type = "pc_chord_type"
  )
}

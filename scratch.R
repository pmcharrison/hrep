library(magrittr)

# Order-insensitive
encode_pc_set <- function(pc_set) {
  sum((2 ^ (11:0)) * (0:11 %in% pc_set))
}

decode_pc_set <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || x < 1 || x > 4095)
    stop("invalid pc_set id")
  x <- as.integer(x)
  binary <- rev(intToBits(x)[1:12] == 1)
  (0:11)[binary]
}

# Order-insensitive, little error checking
encode_pc_chord_type <- function(pc_chord_type) {
  if (length(pc_chord_type) == 0) stop("invalid pc_chord_type")
  1L + sum((2L ^ (10:0)) * (1:11 %in% pc_chord_type))
}

decode_pc_chord_type <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || x < 1 || x > 2047)
    stop("invalid pc_chord_type id")
  x <- as.integer(x)
  binary <- rev(intToBits(x - 1L)[1:11] == 1)
  c(0L, (1:11)[binary])
}

encode_pc_chord <- function(pc_chord) {
  bass <- pc_chord[1]
  chord_type <- (pc_chord - bass) %% 12L
  as.integer(2048L * bass + encode_pc_chord_type(chord_type))
}

decode_pc_chord <- function(x) {
  x <- as.integer(x)
  bass <- x %/% 2048L
  chord_type <- decode_pc_chord_type(x %% 2048L)
  c(bass, sort.int((chord_type[-1L] + bass) %% 12L))
}

pc_set_type_alphabet <- 1:4095 %>%
  purrr::map(decode_pc_set) %>%
  purrr::map(pc_set_norm_form) %>%
  purrr::map_chr(as.character) %>%
  factor()

levels(pc_set_type_alphabet)

pc_set_type_alphabet[encode_pc_set(c(0, 3, 6))] %>% as.integer

length(pc_set_type_alphabet)

pc_set_type_alphabet

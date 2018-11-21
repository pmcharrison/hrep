library(magrittr)

chord_alphabet <- hutil:::get_chord_alphabet()
devtools::use_data(chord_alphabet, overwrite = TRUE)

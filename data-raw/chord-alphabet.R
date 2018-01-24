library(magrittr)

#' This function is very inefficient so its output is stored in data/ and can be accessed by HarmonyStats::chord_alphabet.
get_chord_alphabet <- function(
  format = "by_id"
) {
  assertthat::assert_that(
    assertthat::is.scalar(format),
    format %in% c("by_id", "by_chord", "both")
  )
  lapply(
    0:11,
    function(bass_pc) {
      sets::set_power(
        x = setdiff(0:11, bass_pc)
      ) %>% as.list %>%
        (function(x) {
          lapply(x, function(y) {
            c(bass_pc + 48,
              as.numeric(y) + 60)
          })
        })
    }
  ) %>%
    (function(x) do.call(c, x)) %>%
    (function(chord_alphabet) {
      if (format == "by_id") chord_alphabet else {
        map <- hash::hash(
            keys = lapply(chord_alphabet, HarmonyUtils:::get_chord_storage_key),
            values = seq_along(chord_alphabet)
        )
        if (format == "by_chord") map else list(by_id = chord_alphabet,
                                                by_chord = map)
      }
    })
}

chord_alphabet <- get_chord_alphabet(format = "both")
devtools::use_data(chord_alphabet, overwrite = TRUE)

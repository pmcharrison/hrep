#' This function is very inefficient when max_notes is much less than 12, but at least it's cached.
#' @export
get_chord_alphabet <- function(
  min_notes = 1,
  max_notes = 12,
  cache = TRUE,
  format = "list"
) {
  assertthat::assert_that(
    assertthat::is.scalar(format),
    format %in% c("list", "map")
  )
  cacheR::cache(
    fun_name = "get_chord_alphabet",
    cache = cache,
    cache_root = "cache",
    cache_dir = "HarmonyUtils/get_chord_alphabet",
    expr = {
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
        (function(x) Filter(
          f = function(y) {
            length(y) >= min_notes && length(y) <= max_notes
          }, x = x
        )) %>%
        (function(chord_alphabet) {
          if (format == "list") chord_alphabet else {
            hash::hash(
              keys = lapply(chord_alphabet, get_chord_storage_key),
              values = seq_along(chord_alphabet)
            )
          }
        })
    })
}

get_chord_storage_key <- function(chord) {
  assertthat::assert_that(
    is.numeric(chord)
  )
  paste(chord, collapse = " ")
}

#' @export
encode_chord <- function(chord, chord_alphabet = get_chord_alphabet(format = "map")) {
  encode_chords(chords = list(chord), chord_alphabet = chord_alphabet)
}

#' @export
encode_chords <- function(chords, chord_alphabet = get_chord_alphabet(format = "map")) {
  assertthat::assert_that(
    is.list(chords),
    is(chord_alphabet, "hash")
  )
  keys <- lapply(chords, get_chord_storage_key)
  hash::values(chord_alphabet, keys) %>% unname
}

#' @export
decode_chord <- function(chord, chord_alphabet = get_chord_alphabet(format = "list")) {
  decode_chords(chord, chord_alphabet = chord_alphabet)[[1]]
}

#' @export
decode_chords <- function(chords, chord_alphabet = get_chord_alphabet(format = "list")) {
  assertthat::assert_that(
    is.list(chord_alphabet),
    is.numeric(chords)
  )
  chord_alphabet[chords]
}

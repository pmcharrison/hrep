#' @export
get_midi_params <- function(
  stretched_octave = FALSE,
  tuning_ref_Hz = 440,
  num_harmonics = 11, # including the fundamental
  roll_off = 1, # equivalent to Milne's rho, except for level rather than loudness; levels are multiplied by 1 / (n ^ roll_off)
  frequency_digits = 6,
  fundamental_level_dB = 60 # not used in hutch, seth, or vass
) {
  list(
    stretched_octave = stretched_octave,
    tuning_ref_Hz = tuning_ref_Hz,
    num_harmonics = num_harmonics,
    roll_off = roll_off,
    frequency_digits = frequency_digits,
    fundamental_level_dB = fundamental_level_dB,
    unit_amplitude_in_dB = 60
  )
}

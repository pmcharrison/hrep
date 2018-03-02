# Note - it would be much faster to render everything with sox rather
# than using tuneR.

#' @export
save_wav <- function(x, file, ...) UseMethod("save_wav")
save_wav.chord <- function(x, file, tempo = 60, sample_rate = 44100, bit_rate = 16,
                           num_harmonics = get_midi_params()$num_harmonics,
                           roll_off = get_midi_params()$roll_off) {
  midi <- as.integer(x)
  freq <- HarmonyUtils::convert_midi_to_freq(midi)
  spectrum <- HarmonyUtils::expand_harmonics(
    frequency = freq,
    amplitude = 1,
    dB = FALSE,
    frequency_scale = "Hz",
    num_harmonics
  )
  length_sec <- 60 / tempo
  num_samples <- sample_rate * length_sec
  time <- seq(from = 0,
              to = length_sec,
              length.out = num_samples + 1)[- (num_samples + 1)]
  wav_continuous <- mapply(
    function(frequency, amplitude) {
      amplitude * sin(2 * pi * frequency * time)
    }, spectrum$frequency, spectrum$amplitude
  ) %>% rowMeans
  # Fade in over the first 100 samples
  wav_continuous[1:100] <-
    seq(from = 0, to = 1, length.out = 100) * wav_continuous[1:100]
  # Fade out over the last 100 samples
  wav_continuous[seq(from = length(wav_continuous) - 99,
                     to = length(wav_continuous))] <-
    seq(from = 1, to = 0, length.out = 100) *
    wav_continuous[seq(from = length(wav_continuous) - 99,
                       to = length(wav_continuous))]
  # Convert to discrete representation
  peak <- max(abs(wav_continuous))
  wav_discrete <- wav_continuous %>%
    magrittr::divide_by(peak * 1.1) %>%
    magrittr::multiply_by(2 ^ (bit_rate - 1) - 1) %>%
    round
  # Save
  tmp_file <- tempfile(fileext = ".wav")
  tuneR::writeWave(tuneR::Wave(wav_discrete,
                               samp.rate = sample_rate,
                               bit = bit_rate),
                   filename = tmp_file)
  # Feed through sox to fix encoding problems
  "sox %s %s" %>% sprintf(shQuote(tmp_file), shQuote(file)) %>% system
}

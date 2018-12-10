# TODO: REFACTORING

# Note - it would be much faster to render everything with sox rather
# than using tuneR.

save_wav <- function(x, file, ...) UseMethod("save_wav")
save_wav.pc_chord <- function(x, file,
                              length_sec = 1,
                              sample_rate = 44100,
                              bit_rate = 16,
                              fade_samples = 100,
                              num_harmonics = get_midi_params()$num_harmonics,
                              roll_off = get_midi_params()$roll_off) {
  midi <- as.numeric(x)
  freq <- midi_to_freq(midi)
  spectrum <- expand_harmonics(
    frequency = freq,
    amplitude = 1,
    dB = FALSE,
    frequency_scale = "Hz",
    num_harmonics
  )
  spectrum_to_waveform(frequency = spectrum$x,
                       amplitude = spectrum$y,
                       length_sec = length_sec,
                       sample_rate = sample_rate) %>%
    fade_waveform(fade_samples = fade_samples) %>%
    discretise_waveform(bit_rate = bit_rate) %>%
    save_discrete_waveform(file = file)
}
save_wav.spectrum <- function(x,
                              file,
                              length_sec = 1,
                              sample_rate = 44100,
                              bit_rate = 16,
                              fade_samples = 100) {

}

save_discrete_waveform <- function(x,
                                   file,
                                   sample_rate = attr(x, "sample_rate"),
                                   bit_rate = attr(x, "bit_rate")) {
  tmp_file <- tempfile(fileext = ".wav")
  tuneR::writeWave(tuneR::Wave(x,
                               samp.rate = sample_rate,
                               bit = bit_rate),
                   filename = tmp_file)
  # Feed through sox to fix encoding problems
  "sox %s %s" %>% sprintf(shQuote(tmp_file), shQuote(file)) %>% system
}

spectrum_to_waveform <- function(frequency,
                                 amplitude,
                                 length_sec,
                                 sample_rate = 44100) {
  num_samples <- sample_rate * length_sec
  time <- seq(from = 0,
              to = length_sec,
              length.out = num_samples + 1)[- (num_samples + 1)]
  x <- mapply(
    function(frequency, amplitude) {
      amplitude * sin(2 * pi * frequency * time)
    }, frequency, amplitude
  ) %>% rowMeans
  attr(x, "sample_rate") <- sample_rate
  x
}

fade_waveform <- function(wav_continuous, fade_samples) {
  checkmate::qassert(wav_continuous, "N")
  checkmate::qassert(fade_samples, "X1")
  stopifnot(fade_samples < length(wav_continuous))
  # Fade in over the first n samples
  wav_continuous[1:fade_samples] <-
    seq(from = 0, to = 1, length.out = fade_samples) * wav_continuous[1:fade_samples]
  # Fade out over the last n samples
  wav_continuous[seq(length.out = fade_samples,
                     to = length(wav_continuous))] <-
    seq(from = 1, to = 0, length.out = fade_samples) *
    wav_continuous[seq(length.out = fade_samples,
                       to = length(wav_continuous))]
  wav_continuous
}

discretise_waveform <- function(x, bit_rate = 16) {
  peak <- max(abs(x))
  y <- x %>%
    magrittr::divide_by(peak * 1.1) %>%
    magrittr::multiply_by(2 ^ (bit_rate - 1) - 1) %>%
    round
  attr(y, "bit_rate") <- bit_rate
  y
}

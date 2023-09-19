##Rick Astley - Never Gonna Give You Up##
library(audio)
library(dplyr)
library(ggplot2)
RA_notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
pitch <- paste(
  "D5 E5 A",
  "E5 F#5 A5 G5 F#5",
  "D5 E5 A Rest",
  "A A B D5 D5",
  "D5 E5 A",
  "E5 F#5 A5 G5 F#5",
  "D5 E5 A Rest",
  "A A B D5 D5")
RA_chorus <- paste(
  "B C#5 ")


RA_duration <- c(rep(c(1.25, 1.25, 1, 1.25, 1.25, 0.25, 0.25, 0.5, 1.25, 1.25, 2.25, 1, 0.25, 0.25, 0.25, 0.5, 0.25), 2))
rick <- data.frame(pitch = strsplit(pitch, " ")[[1]],
                      duration = RA_duration)
rick <- 
  rick %>% 
  mutate(octave = substring(pitch, nchar(pitch)) %>%
         {suppressWarnings(as.numeric(.))} %>%
         ifelse(is.na(.), 4, .),
       note = RA_notes[substr(pitch, 1, 1)],
       note = note + grepl("#", pitch) -
         grepl("b", pitch) + octave * 12 +
         12 * (note < 3),
       freq = 2 ^ ((note - 60) / 12) * 440)
tempo <- 114
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}
rick_wave <- mapply(make_sine, rick$freq, rick$duration) %>%
  do.call("c", .)

ggplot(data = rick, aes(y = pitch, x = duration)) + geom_violin() + scale_x_binned(limits = c(0, 4))
play(rick_wave)

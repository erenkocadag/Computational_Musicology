library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(showtext)
library(plotly)
library(compmus)

baba <- get_playlist_audio_features("", "37i9dQZF1DX9azIhpBmNbd")
saveRDS(object = baba,file = "data/baba-data.RDS")
eski <- get_playlist_audio_features("", "12VU80QKYGednqNghHrWNW")
saveRDS(object = eski,file = "data/eski-data.RDS")
beskr <- get_playlist_audio_features("", "37i9dQZF1DXciCKvzkUxfS")
saveRDS(object = beskr,file = "data/beskr-data.RDS")
beskp <- get_playlist_audio_features("", "37i9dQZF1DXduWUvte9ZYi?")
saveRDS(object = beskp,file = "data/beskp-data.RDS")

muslum2 <-
  get_tidy_audio_analysis("4cHrCNJTdMfbtY0fjc5ged") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )
saveRDS(object = muslum2,file = "data/muslum2-data.RDS")

haydi <-
  get_tidy_audio_analysis("3XTzXWtABSuYqAvOso5YP9") |>
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )
saveRDS(object = haydi,file = "data/haydi-data.RDS")
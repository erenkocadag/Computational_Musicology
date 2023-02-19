library(tidyverse)
library(spotifyr)
library(ggplot2)
Sys.setenv(SPOTIFY_CLIENT_ID = 'c165e1d0717c4bc1a4b10fad65d016e5')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '7771634e58dd4c379280db7d30afbf25')

access_token <- get_spotify_access_token()
#juditha <- get_track_audio_features(c("2M5b9YLAgFroqWzeaZf86e", "3DBKc4ioGnMQLlbGQcFDIO"))
#alla <- get_album_tracks("7oI0E3DdTbD85rhMg19GSU")
#gilberto <- get_artist_audio_features("gilberto gil")
#ecm <- get_playlist_audio_features("", "1zN4nK6oHMo2dei2WWPtSL")

#ecm |>
#  summarise(
#    mean_speechiness = mean(speechiness),
#    mean_acousticness = mean(acousticness),
#    mean_liveness = mean(liveness),
#    sd_speechiness = sd(speechiness),
#    sd_acousticness = sd(acousticness),
#    sd_liveness = sd(liveness),
#    median_speechiness = median(speechiness),
#    median_acousticness = median(acousticness),
#    median_liveness = median(liveness),
#    mad_speechiness = mad(speechiness),
#    mad_acousticness = mad(acousticness),
#    mad_liveness = mad(liveness)
#  )

disney <- get_playlist_audio_features("", "37i9dQZF1DX8C9xQcOrE6T")
disney |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )
ggplot(disney, aes(tempo, danceability, size = energy, color = valence)) +
  theme_classic() +
  geom_point()

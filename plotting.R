# loading in all libraries
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(showtext)
library(plotly)
library(compmus)

font_add_google("Inter", "Inter")
showtext_auto()

# getting access to the spotify API
# Sys.setenv(SPOTIFY_CLIENT_ID = 'c165e1d0717c4bc1a4b10fad65d016e5')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '7771634e58dd4c379280db7d30afbf25')

# access_token <- get_spotify_access_token()

# retrieving the audio features of my corpus
baba <- get_playlist_audio_features("", "37i9dQZF1DX9azIhpBmNbd")
eski <- get_playlist_audio_features("", "12VU80QKYGednqNghHrWNW")
beskr <- get_playlist_audio_features("", "37i9dQZF1DXciCKvzkUxfS")
beskp <- get_playlist_audio_features("", "37i9dQZF1DXduWUvte9ZYi?")

# merging the playlists and other preprocessing
eski <- mutate(eski, playlist_name = "Arabesk")
baba <- mutate(baba, playlist_name = "Arabesk")
merged <- merge(beskr, beskp, all = TRUE)
merged <- merge(merged, baba, all = TRUE)
merged <- merge(merged, eski, all = TRUE)
merged <- merged[,!names(merged) %in% c('playlist_img','playlist_owner_name',
                                        'playlist_owner_id','playlist_id',
                                        'track.id','added_at',
                                        'primary_color','added_by.id',
                                        'added_by.type','added_by.uri',
                                        'added_by.href','added_by.external_urls.spotify',
                                        'track.artists','track.disc_number',
                                        'track.episode','track.is_local',
                                        'is_local','track.href',
                                        'track.preview_url','video_thumbnail.url',
                                        'track.album.type','track.album.uri',
                                        'track.external_ids.isrc','track.external_urls.spotify',
                                        'track.album.external_urls.spotify','track.uri',
                                        'track.track','track.type',
                                        'track.album.href','track.album.id',
                                        'track.album.images','track.album.total_tracks',
                                        'track.track_number','track.available_markets',
                                        'track.album.available_markets')]

# the actual plotting
val <- merged %>%
  group_by(playlist_name) %>%
  summarize(mean_valence = mean(valence)) %>%
  ggplot(aes(playlist_name, mean_valence, fill = playlist_name)) +
    geom_col() +
    geom_text(aes(label=round(mean_valence, 2)), vjust = -0.5) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = "Playlist", y = " Mean valence", title = "The difference in the valence of the playlists is small") +
    theme_bw() + 
    theme( text = element_text(family = "Inter"),
           legend.position = 'none',
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank()) +
    scale_y_continuous(breaks=seq(0,1,1))

ggplotly(val)

scatter <- ggplot(merged, aes(danceability, acousticness)) +
  geom_point() +
  # geom_smooth() +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_bw() +
  theme(
    text = element_text(family = "Inter"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(x = "Danceability", y = "Acousticness", title = "Classic Arabesk songs seem to be more acoustic and less danceable than the two subgenres") +
  facet_wrap(~playlist_name)

ggplotly(scatter)


pop <- ggplot(merged, aes(tempo, track.popularity, color=playlist_name)) +
        geom_violin() +
        geom_boxplot() +
        labs(x = "Tempo", y = "Track popularity", title = "Older Arabesk music currently is less popular than the subgenres") +
        theme_bw() +
        theme(
          legend.position = 'none',
          text = element_text(family = "Inter"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
        facet_wrap(~playlist_name)

fig <- plot_ly(y = merged$playlist_name, x = merged$track.popularity, type = "box", color = ~merged$playlist_name)
fig <- fig %>% layout(title = "Older Arabesk music currently is less popular than the subgenres", xaxis = list(title="Track popularity"), yaxis = list(showticklabels=FALSE))

fig

muslum <-
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

chroma1 <- muslum |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()


#ggplotly(chroma1)
         
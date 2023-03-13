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

muslum <-
  get_tidy_audio_analysis("4cHrCNJTdMfbtY0fjc5ged") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)


gripin <-
  get_tidy_audio_analysis("2SGw7SCQWp1vdTAqiy9LnZ") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

chroma1 <- compmus_long_distance(
  muslum |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  gripin |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  feature = pitches,
  method = "manhattan"
) |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Müslüm Gürses", y = "Gripin", title = "Distance Matrix of the pitches of the song \'Nilüfer\'") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)

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
timb <- muslum2 |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(title = "Self similarity Matrix of the timbre of Müslüm\'s Nilüfer") +
  labs(x = "", y = "")

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

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

haydi2 <- haydi |> 
  compmus_match_pitch_template(
    chord_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title = "Chordogram of Seni Yazdım")
ggplotly(haydi2)

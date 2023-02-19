# loading in all libraries
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(showtext)

font_add_google("Inter", "Inter")
showtext_auto()

# getting access to the spotify API
# Sys.setenv(SPOTIFY_CLIENT_ID = 'c165e1d0717c4bc1a4b10fad65d016e5')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '7771634e58dd4c379280db7d30afbf25')

# access_token <- get_spotify_access_token()

# retrieving the audio features of my corpus
baba <- get_playlist_audio_features("", "37i9dQZF1DX9azIhpBmNbd")
beskr <- get_playlist_audio_features("", "37i9dQZF1DXciCKvzkUxfS")
beskp <- get_playlist_audio_features("", "37i9dQZF1DXduWUvte9ZYi?")

# merging the playlists and removing irrelevant features to make plots
merged <- merge(beskr, beskp, all = TRUE)
merged <- merge(merged, baba, all = TRUE)
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
merged %>%
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

ggplot(merged, aes(danceability, acousticness)) +
  geom_point() +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_bw() +
  theme(
    text = element_text(family = "Inter"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(x = "Danceability", y = "Acousticness", title = "Classic Arabesk songs seem to be more acoustic and less danceable than the two subgenres") +
  facet_wrap(~playlist_name)
         
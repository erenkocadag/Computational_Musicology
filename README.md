# Computational Musicology

A repository used as a course portfolio for the Computational Musicology course taught at the University of Amsterdam.

### PLEASE DO NOT SHOW THIS PORTFOLIO IN CLASS

# A description of my corpus:

My corpus consists of playlists made by Spotify that revolve around the arabesk music genre. Arabesk is a popular Turkish music genre that emerged in the 1960s and blends traditional Turkish music with elements of Middle Eastern and Southeast European music. The first playlist consists of Arabesk music that is considered more old-school and classic. It's titled 'Babalar'. The second and third playlists are of subgenres of Arabesk music that emerged decades after Arabesk music arrived. The genres are Arabesk pop and Arabesk rap. Their respective playlists are "Besk Pop" and "Besk Rap".

I chose this corpus, because I don't know enough about this genre and want to learn more. What's interesting about Arabesk is that it has changed tremendously throughout the years.

My main interest of points to compare are how the chosen subgenres are similar or different to classic Arabesk music. It could also be interesting to see the differences between songs from each decade.

I believe that the playlists cover their genres fairly well, as Spotify has created them with the purpose of doing so. A strength would be the variety in artists that are featured in the playlists. A weakness could be the size of the corpus.

A typical Arabesk track could be UnutamadD1m (KaC' Kadeh KD1rD1ldD1) by MC\<slC\<m GC\<rses, due to the melancholic tone of the song. A track that could be quite atypical might be Alev Alev by Hayat, as part of it is in German and does not share the typical sentiment of older Arabesk songs. Thus, it makes sense for it to be in the Besk rap playlist.

## First Visualisations

![](https://raw.githubusercontent.com/erenkocadag/Computational_Musicology/main/Plots/acous.png)

The scatter plot above plots the acousticness of the playlists on the Y-axis and the danceability of the playlists on the X-axis. This is done in three subplots for each of the playlists. When looking at the pattern of the dots, it becomes clear that classic Arabesk music tends to have a lower danceability rating and a higher acousticness rating in comparison to the two subgenres (Arabesk Pop and Arabesk Rap). This shows some of the differences of the more recent music in the subgenres in comparison to the older music.

![](https://raw.githubusercontent.com/erenkocadag/Computational_Musicology/main/Plots/valence2.png)

The plot above shows the mean valence for each of the playlists used. The difference between the values are actually much smaller than I expected them to be. Prior to this plot, I would have expected the classic Arabesk playlist (Babalar) to have somewhat of a lower valence than the other two playlists. Perhaps the mean was not the best property to take of the valence feature, due to its sensitivity to outliers. For next week I might come back to this with a different approach.

### Links to the playlists:

All data that is used in these visualisations are from the Spotify API with which I gathered information about the following playlists:

[Babalar](https://open.spotify.com/playlist/37i9dQZF1DX9azIhpBmNbd?si=6db51b6addd848c0)

[Besk Pop](https://open.spotify.com/playlist/37i9dQZF1DXduWUvte9ZYi?si=81f7a3ba11a4430b)

[Besk Rap](https://open.spotify.com/playlist/37i9dQZF1DXciCKvzkUxfS?si=c9b1a17357b9411d)

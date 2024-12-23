rm(list = ls())
library(ggplot2)
library(tidyverse)

load("song_shows.RData")

song_show_f <- song_show %>%
  filter(
    !(city.y %in% c("Saint Charles",
                    "Austin 1", 
                    "New Orleans", 
                    "Salt Lake City",
                    "Luton",
                    "Barcelona")))


n_songs <- song_show_f   %>%
  group_by(city.y) %>%
  summarise(
    n_songs = n(),
    song = ifelse(any("Campus" %in% sets), 1, 0)
  ) %>%
  ungroup()

n_songs$show_num <- rev(1:52)

plot(rev(n_songs$n_songs), type = 'l')

ggplot(data=n_songs, aes(x=show_num, y=n_songs, group=1, col)) +
  geom_point()


correlation <- cor(n_songs$song, n_songs$n_songs, use = "complete.obs")


plot_ly(data = n_songs, x = ~show_num, y = ~n_songs, color = ~song)


song_correlation_results <- lapply(unique(song_show$sets), function(song) {
  song_data <- song_show_f %>%
    group_by(city.y) %>%
    mutate(
      set_length = n(),                # Length of each set
      has_song = ifelse(song %in% sets, 1, 0)  # 1 if the song is present, else 0
    ) %>%
    ungroup()
  
  # Calculate correlation
  correlation <- cor(song_data$has_song, song_data$set_length, use = "complete.obs")
  
  # Return results as a dataframe
  data.frame(
    song = song,
    correlation = correlation
  )
})


song_correlation_results <- bind_rows(song_correlation_results)


min(song_correlation_results$correlation,na.rm = T)

save(song_correlation_results, file= "data/songcorrsetlength.RData")




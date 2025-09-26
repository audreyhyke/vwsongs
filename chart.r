library(gt)
library(scales)

my_shows <- c("Chicago 2","Minneapolis 1", "Minneapolis 2", "Milwaukee", "Saint Charles", "Indianapolis","Cincinnati", "New York 1", "New York 2", "London 1", "London 2", "London 3", "London 4", "Montclair 1", "Montclair 2", "Montclair 3", "Montclair 4")

my_vwc <- vwc[vwc$city_num %in% my_shows,]

my_songs <- my_vwc %>%
  select(sets) %>%
  group_by(sets) %>%
  summarize(
    freq = n()
  ) %>%
  ungroup()

albums <- albums[,-1]

my_song_counts <- albums %>%
  left_join(my_songs,by="sets")

my_song_counts$freq <- ifelse(is.na(my_song_counts$freq),0,my_song_counts$freq)


msc_wide <- my_song_counts %>%
  group_by(album) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = row,
    names_from = album,
    values_from = c(sets, freq)
  )


album_colors <- c(
  "VW" = "orange",
  "Contra" = "purple",
  "MVOTC" = "gray",
  "FOTB" = "lightgreen",
  "OGWAU" = "darkblue"
)


msc_wide %>%
  gt() %>%
  # add album as column spanners
  tab_spanner(label = "VW", columns = c('sets_Vampire Weekend', 'freq_Vampire Weekend')) %>%
  tab_spanner(label = "Contra", columns = c('sets_Contra', 'freq_Contra')) %>%
  tab_spanner(label = "MVOTC", columns = c('sets_Modern Vampires of the City', 'freq_Modern Vampires of the City')) %>%
  tab_spanner(label = "FOTB", columns = c('sets_Father of the Bride', 'freq_Father of the Bride')) %>%
  tab_spanner(label = "OGWAU", columns = c('sets_Only God Was Above Us', 'freq_Only God Was Above Us')) %>%
  # style album headers with background colors
  tab_style(
    style = cell_fill(color = album_colors["VW"]),
    locations = cells_column_spanners(spanners = "VW")
  ) %>%
  tab_style(
    style = cell_fill(color = album_colors["Contra"]),
    locations = cells_column_spanners(spanners = "Contra")
  ) %>%
  tab_style(
    style = cell_fill(color = album_colors["MVOTC"]),
    locations = cells_column_spanners(spanners = "MVOTC")
  ) %>%
  tab_style(
    style = cell_fill(color = album_colors["FOTB"]),
    locations = cells_column_spanners(spanners = "FOTB")
  ) %>%
  tab_style(
    style = cell_fill(color = album_colors["OGWAU"]),
    locations = cells_column_spanners(spanners = "OGWAU")
  ) %>%
  # gradient fills per album
  data_color(
    columns = 'freq_Vampire Weekend',
    colors = col_numeric(c("white", album_colors["VW"]), domain = NULL)
  ) %>%
  data_color(
    columns = 'freq_Contra',
    colors = col_numeric(c("white", album_colors["Contra"]), domain = NULL)
  ) %>%
  data_color(
    columns = 'freq_Modern Vampires of the City',
    colors = col_numeric(c("white", album_colors["MVOTC"]), domain = NULL)
  ) %>%
  data_color(
    columns = 'freq_Father of the Bride',
    colors = col_numeric(c("white", album_colors["FOTB"]), domain = NULL)
  ) %>%
  data_color(
    columns = 'freq_Only God Was Above Us',
    colors = col_numeric(c("white", album_colors["OGWAU"]), domain = NULL)
  ) 




my_song_counts %>% 
  filter(album != "Other") %>%
  gt() %>%
  tab_header(title = md("VW Songs I've Seen Live"),
             subtitle = md("just for ogwau tho"))


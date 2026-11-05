rm(list = ls())
library(tidyverse)
load("data/song_shows.RData")


get_shows <- function(songs,song_show){
  
  song_show %>%
    filter(
      sets %in% songs
    ) %>%
    select(
      city.y
    ) %>%
    return()
  
}

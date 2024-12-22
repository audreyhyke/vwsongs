rm(list = ls())
library(tidyverse)
library(dplyr)

load("data/songshow.RData")


get_setlist <- function(show,song_show_wide) {
  
  song_show_wide %>%
    select(
      all_of(show)
    ) %>%
    unlist() %>%
    return() 
  
  #return(unname(unlist(song_show_wide[show])))
  
}


get_setlist(c("Amsterdam","Paris"),song_show_wide)



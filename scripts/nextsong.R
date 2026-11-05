rm(list = ls())

load("data/nextsongs.RData")


get_next_desc <- function(song,song_next_per){
  return(song_next_per[[song]])
}

get_next_desc("Capricorn", song_next_per)


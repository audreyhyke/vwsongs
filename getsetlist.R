rm(list = ls())
library(tidyverse)
library(dplyr)

load("data/songshow.RData")


get_setlist <- function(show,song_show_wide) {
  
  song_show_wide %>%
    select(
      all_of(show)
    ) %>%
    return() 
  
  #return(unname(unlist(song_show_wide[show])))
  
}


s <- get_setlist(c("Amsterdam","Paris"),song_show_wide)



s <- song_show_wide


max_length <- 41

max(sapply(s[[1]], length))

sd <- lapply(s[[1]], unlist)

# Function to pad lists with "" for shorter ones
pad_list <- function(lst, max_len) {
  length(lst) <- max_len
  lst[is.null(lst)] <- ""
  return(lst)
}

# Apply the padding function to each list in the list of lists
padded_lists <- lapply(s, pad_list, max_len = max_length)

# Convert the padded list of lists into a data frame
df <- as.data.frame(do.call(cbind, padded_lists), stringsAsFactors = FALSE)


df_songs <- song_show_wide

max_length <- max(sapply(df_songs, function(x) length(unlist(x))))

# Function to pad lists with "" for shorter ones
pad_list <- function(lst, max_len) {
  # Extend list with "" if shorter
  length(lst) <- max_len
  lst[is.null(lst)] <- ""
  return(lst)
}

# Apply the padding function to each column's list of songs
df_padded <- as.data.frame(lapply(df_songs, function(x) {
  # Pad each list to the maximum length
  pad_list(unlist(x), max_length)
}), stringsAsFactors = FALSE)
colnames(df_padded) <- colnames(df_songs)


song_show_wide <- df_padded










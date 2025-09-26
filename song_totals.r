get_song_totals <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    rename(
      song = sets
    ) %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  VW <- na.omit(piv_total[c("song","Vampire Weekend")])
  C <- na.omit(piv_total[c("song","Contra")])
  MVOTC <- na.omit(piv_total[c("song","Modern Vampires of the City")])
  FOTB <- na.omit(piv_total[c("song","Father of the Bride")])
  OGWAU <- na.omit(piv_total[c("song","Only God Was Above Us")])
  
  VW_gt <- VW %>% gt() %>%
    data_color(
    columns = 'Vampire Weekend',
    target_columns = everything(),
    colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
  ) %>%
    tab_options(
      table.font.size = 12
    )
  
  C_gt <- C %>% gt() %>%
    data_color(
      columns = 'Contra',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )
  
  MVOTC_gt <- MVOTC %>% gt() %>%
    data_color(
      columns = 'Modern Vampires of the City',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )
  
  FOTB_gt <- FOTB %>% gt() %>%
    data_color(
      columns = 'Father of the Bride',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )
  
  OGWAU_gt <- OGWAU %>% gt() %>%
    data_color(
      columns = 'Only God Was Above Us',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )
  
  return(c(VW_gt,C_gt,MVOTC_gt,FOTB_gt,OGWAU_gt))
}



shows = unique(vwc$choice_name)[3:6]

get_song_totals(shows,vwc,albums)



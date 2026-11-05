rm(list = ls())

#####
# libraries


# basic

library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(stringr)
library(ggplot2)
library(plotly)

# ridges
library(ggridges)
library(hdrcde)

# given
library(pheatmap)

# kmeans
library(stats)

# streamgraph
library(ggstream)

# dag
library(igraph)

######
# data + functions

# basic
load("data/albumcolors.RData")
load("data/song_shows.RData")
load("data/vwc_pt2_F.RData")

# ridges
load("data/ridges.RData")

# next songs
load("data/nextsongs.RData")
get_next_desc <- function(song,song_next_per){
  return(song_next_per[[song]])
}

# given
load("data/givenheatmap.RData")
percent_given <- function(given,song1,song2){
  return(given[which(colnames(given) == song1),which(colnames(given) == song2)])
}
all_percent_given <- function(given,song){
  return(unname(cbind(rownames(given),given[,which(colnames(given) == song)])))
}

# piechart
load("data/setlistalbums.RData")
get_piechart <- function(shows,setlist_albums_songs){
  
  city_songs <- setlist_albums_songs %>%
    filter(
      choice_name %in% shows
    ) %>%
    select(
      album, sets, col
    )
  
  sum_songs <- city_songs %>%
    group_by(
      album
    ) %>%
    mutate(
      n_album = n(),
      album_per = round(n_album / nrow(city_songs),2)
    ) %>%
    ungroup() %>%
    group_by(sets) %>%
    mutate(
      num_song = n()
    ) %>%
    ungroup() %>%
    mutate(
      a_label = paste(album,", ",album_per*100,"%",sep = ""),
      s_label = paste(sets,", ",num_song,sep = "")
    )
  
  a <- sum_songs$album
  a_p <- sum_songs$a_label
  s_n <- sum_songs$s_label
  b <- sum_songs$sets
  c<- sum_songs$num_song
  
  l<- rbind(unique(cbind(a,a_p)),cbind(b,s_n))
  
  p <- c(rep("",length(unique(a))),a)
  
  num_albums <- unique(cbind(sum_songs$album,sum_songs$n_album))[,2]
  
  v <- as.numeric(c(num_albums,sum_songs$num_song))
  
  i <- c(unique(a),paste(a,"-",b,sep = ""))
  
  a <- na.omit(as.data.frame(c(l[1:length(unique(a))],p[length(unique(a))+1:length(p)])))
  colnames(a) <- "album"
  
  c <- left_join(a,albumcolors,by="album")
  
  h <- paste("<b>",l,"</b><br>",ifelse(l %in% a,paste(a_p*100,"%<br>",a_n," times"),""))
  
  argh <- unique(as.data.frame(cbind(l,p,v,i,c)))
  
  argh <- arrange(argh,desc(v))
  
  fig1 <- plot_ly(argh)
  
  fig1 <- fig1 %>% add_trace(
    labels = ~a,
    parents = ~p,
    values = ~v,
    type = 'sunburst',
    insidetextorientation='radial',
    branchvalues='total',
    hovertemplate = ~a_p
  ) %>%
    layout(
      extendsunburstcolors = TRUE,
      colorway = ~col)
  
  fig1
  
  
}

# chart
get_song_totalsVW <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  VW <- na.omit(piv_total[c("song","Vampire Weekend")])
  VW_gt <- VW %>% gt() %>%
    data_color(
      columns = 'Vampire Weekend',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )%>%
    tab_style(
      style = list(cell_fill(color = "orange"), cell_text(color = "black")),
      locations = cells_body(
        columns = everything(),
        rows = `Vampire Weekend` > length(shows)
      ))
  
  return(VW_gt)
}
get_song_totalsC <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  
  C <- na.omit(piv_total[c("song","Contra")])
  
  
  C_gt <- C %>% gt() %>%
    data_color(
      columns = 'Contra',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  return(C_gt)}
get_song_totalsM <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  MVOTC <- na.omit(piv_total[c("song","Modern Vampires of the City")])
  
  
  MVOTC_gt <- MVOTC %>% gt() %>%
    data_color(
      columns = 'Modern Vampires of the City',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  
  return(MVOTC_gt)}
get_song_totalsF <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  FOTB <- na.omit(piv_total[c("song","Father of the Bride")])
  
  
  
  FOTB_gt <- FOTB %>% gt() %>%
    data_color(
      columns = 'Father of the Bride',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  return(FOTB_gt)}
get_song_totalsO <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  OGWAU <- na.omit(piv_total[c("song","Only God Was Above Us")])
  
  OGWAU_gt <- OGWAU %>% gt() %>%
    data_color(
      columns = 'Only God Was Above Us',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  return(OGWAU_gt)}

# setlist
get_setlist <- function(choice, song_show_wide) {
  choice <- intersect(choice, names(song_show_wide))
  if (length(choice) == 0) return(data.frame())
  maxlen <- max(sapply(choice, function(nm) length(song_show_wide[[nm]][[1]])))
  cols <- lapply(choice, function(nm) {
    x <- song_show_wide[[nm]][[1]]
    length(x) <- maxlen         # pad with NA to equal length
    x
  })
  df <- as.data.frame(setNames(cols, choice), check.names = FALSE)
  return(df)
}

# streamgraph
load("data/album_per.RData")
load("data/streamgraphplots.RData")

# dag
get_dag <- function(shows,vwc){
  
  songlist <- vwc[,c("choice_name","sets","song_num")]
  
  setlists_1 <- songlist[which(songlist$choice_name %in% shows),]
  
  setlists_1 <- setlists_1 %>%
    group_by(choice_name) %>%
    mutate(song_num = row_number()) %>%
    ungroup()
  
  song_pos_avg <- setlists_1 %>%
    select(
      sets, song_num
    ) %>%
    group_by(
      sets
    ) %>%
    summarize(
      avg_pos = max(song_num)
    ) %>%
    mutate(
      name = sets
    )
  
  edges <- data.frame(From = NA, To = setlists_1$sets[1], Show = setlists_1$choice_name[1])
  
  for(i in 1:(nrow(setlists_1)-1)){
    edges <- rbind(edges,data.frame(From = setlists_1$sets[i], To = setlists_1$sets[i+1], Show = setlists_1$choice_name[i]))
  }
  
  for(show in shows){
    edges <- edges[-min(which(edges$Show == show)),]
  }
  
  edges <- na.omit(edges)
  
  
  starting_songs <- setlists_1$sets[setlists_1$song_num==1]
  
  graph <- graph_from_data_frame(edges, directed = TRUE)
  
  vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)
  
  layout <- layout_as_tree(graph) 
  
  show_colors <- setNames(sample(rainbow(length(shows))), sort(unique(setlists_1$choice_name)))
  
  
  edge_colors <- show_colors[edges$Show]
  
  plot(
    graph,
    layout = layout_with_sugiyama,
    vertex.label.font = vertex_font_weight,
    vertex.label.color = "black",
    edge.arrow.size = 0.2,
    edge.color = edge_colors,
    vertex.shape = "rectangle",
    vertex.label.cex=.8,
    vertex.color = "white",
    vertex.frame.color = "white",
    main = "Setlist Directed Graph",
    asp = 1.5,
    arrow.mode = 0
  )
  legend("topleft",bty = "n",
         legend=unique(names(edge_colors)),
         fill=unique(edge_colors), border=NA,
         cex = 0.7)
  
}


#####
# ui


ui <- page_navbar(
  title = "VW OGWAU Tour Statistics",
  theme = bs_theme(version = 5),
  
  nav_panel("Home",
            layout_column_wrap(
              width = 1,
              card(full_screen = TRUE, h2("Home"), 
                   h5("I was super bored and missing the OGWAU tour so here are some statistics on it. I do not claim to be smart or knowledgeable in statistics even though I got a degree in it."))
            )
  ),
  
  nav_panel("Setlists", 
            card(full_screen = TRUE, 
                 h2("Setlists"), 
                 "CONTENT HERE")),
  
  nav_panel("Song Totals", 
            card(full_screen = TRUE, 
                 h2("Song Totals"), "CONTENT HERE")),
  
  nav_panel("Clusters", 
            card(full_screen = TRUE, 
                 h2("Clusters"), "CONTENT HERE")),
  
  nav_panel("Next Song", 
            card(full_screen = TRUE, 
                 h2("Next Song"), "CONTENT HERE")),
  
  
  nav_menu("Visualizations",
           
           nav_panel("Streamgraph", 
                     card(full_screen = TRUE, 
                          h2("Streamgraph"), "CONTENT HERE")),
           
           nav_panel("Albums over time", 
                     card(full_screen = TRUE, 
                          h2("Albums over time"), "CONTENT HERE")),
           
           nav_panel("Album Percentages", 
                     card(full_screen = TRUE, 
                          h2("Album Percentages"), "CONTENT HERE")),
           
           nav_panel("Heatmap", 
                     card(full_screen = TRUE, 
                          h2("Heatmap"), "CONTENT HERE")),
           
           nav_panel("Dendrogram", 
                     card(full_screen = TRUE, 
                          h2("Dendrogram"), "CONTENT HERE"))
  ),
  
  nav_menu("Comparisons",
           
           nav_panel("Two Songs", 
                     card(full_screen = TRUE, 
                          h2("Two Songs"), "CONTENT HERE")),
           
           nav_panel("Overlap", 
                     card(full_screen = TRUE, 
                          h2("Overlap"), "CONTENT HERE")),
           
           nav_panel("Deep Cuts", 
                     card(full_screen = TRUE, 
                          h2("Deep Cuts"), "CONTENT HERE")),
           
           nav_panel("Order of Songs", 
                     card(full_screen = TRUE, 
                          h2("Order of Songs"), "CONTENT HERE"))
  )
)

server <- function(input, output, session) {
  # outputs here
}

shinyApp(ui, server)




rm(list = ls())
library(tidyverse)
library(dplyr)
library(treemap)
library(ggplot2)
library(plotly)

load("data/setlistalbums.RData")
load("data/albumcolors.RData")

get_piechart <- function(shows,setlist_albums_songs){
  
  city_songs <- setlist_albums_songs %>%
    filter(
      city.y %in% shows
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

shows = unique(setlist_albums_songs$city.y)

get_piechart(shows,setlist_albums_songs)


######

treemap(sum_songs,
        index = c("a_label", "s_label"), vSize = "album_per", type = "color", vColor = "col",
        fontsize.labels = c(12, 6),
        fontcolor.labels = c("white", "grey"), 
        fontface.labels = c(2, 3), 
        bg.labels = c("transparent"),
        align.labels = list(
          c("center", "top"),
          c("right", "bottom")),
        overlap.labels = 1, 
        inflate.labels = F, 
        border.col = c("white", "black"),
        aspRatio = 6/3
        
)

######
# ggplot(albums, aes(area = album_per, fill = col, label = album)) +
#   geom_treemap() +
#   geom_treemap_text()
# 
# 
# 
# # Package
# library(treemap)
# 
# # Plot
# 
# albums %>%
#   drop_na() %>%
#   treemap(  # data
#     index="album",
#     vSize="album_per",
#     type="index")
# 
# lbls <- paste(albums$album, "\n",albums$album_per)
# 
# 
# pie(albums$album_per,col = albums$col,labels = lbls,border = "white")
# 
# 
# 
# 
# shows = c("Amsterdam","Paris","London 4")
# 
# 
# 
# 
# group <- c(rep("group-1", 4), rep("group-2", 2), rep("group-3", 3))
# subgroup <- paste("subgroup", c(1, 2, 3, 4, 1, 2, 1, 2, 3), sep = "-")
# value <- c(13, 5, 22, 12, 11, 7, 3, 1, 23)
# data <- data.frame(group, subgroup, value)
# 
# 
# 
# vwd %>%
#   select(
#     city.y,album,sets
#   ) %>%
#   filter(
#     city.y %in% c("Amsterdam","Paris","London 4")
#   ) %>%
#   left_join(albums,by = "album")
# 
# 
# 
# city_songs <- setlist_albums_songs %>%
#   filter(
#     city.y %in% shows
#   ) %>%
#   select(
#     album, sets
#   )
# 
# sum_songs <- city_songs %>%
#   group_by(
#     album
#   ) %>%
#   summarize(
#     n_album = n()
#   ) %>%
#   ungroup() %>%
#   mutate(
#     album_per = round(n_album / sum(n_album),2)
#   )
# 
# sets_album_per <- city_songs %>%
#   left_join(sum_songs,by = "album") %>%
#   left_join(albumcolors, by = "album")
# 
# 
# treemap(sets_album_per,
#         index = c("album", "sets"), vSize = "album_per", type = "color", vColor = "col",
#         fontsize.labels = c(20, 8), # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
#         fontcolor.labels = c("white", "grey"), # Color of labels
#         fontface.labels = c(2, 3), # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
#         bg.labels = c("transparent"), # Background color of labels
#         align.labels = list(
#           c("center", "top"),
#           c("right", "bottom")
#         ), # Where to place labels in the rectangle?
#         overlap.labels = 1, # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
#         inflate.labels = F, # If true, labels are bigger when rectangle is bigger.
#         border.col = c("black", "grey"),
#         labels = c(paste(album,albumper),paste(sets,))
# )
# 
# 
# 
# 
# 
# 
# filtered_songs <- setlist_albums_songs %>%
#   filter(
#     city.y %in% shows
#   ) %>%
#   select(
#     album, sets
#   ) %>%
#   group_by(
#     album
#   ) %>%
#   summarize(
#     n_album = n()
#   ) %>%
#   ungroup()
# 
# shows = c("Amsterdam","Paris","London 4","Minneapolis 1")
# 
# get_treemap(c("Amsterdam","Paris","London 4","Minneapolis 1"),setlist_albums_songs)
# 
# library(packcircles)
# 
# 
# cPL <- circleProgressiveLayout(sum_songs,"album_per")
# 
# cLV <- circleLayoutVertices(cPL)
# 
# cLV$id
# 
# ggplot(data = cLV, aes(x, y, group = id)) +
#   geom_polygon(colour = "black", fill = "grey90") +
#   coord_equal() +
#   theme_void()
# 
# 
# library(ggraph)
# library(igraph)
# 
# 
# albums <- as.data.frame(unique(sum_songs$album))
# names(albums) <- c("album")
# 
# relations <- cbind(sum_songs$album,sum_songs$sets)
# colnames(relations) <- c("from","to")
# 
# vertice <- cbind(sum_songs$sets,seq(1:nrow(sum_songs)))
# 
# colnames(vertice) <- c("name","size")
# 
# gfdf <- graph_from_data_frame(relations)
# 
# all <- rbind(relations,vertice)
# 
# gfdf <- graph_from_data_frame(all)
# 
# 
# 
# 
# 
# 
# # Make the plot
# ggraph(gfdf, layout = 'circlepack') + 
#   geom_node_circle() +
#   theme_void()
# 
# ggraph(gfdf, 'partition', circular = TRUE) + 
#   geom_node_arc_bar(aes(fill = depth)) +
#   theme_void() +
#   theme(legend.position="none")
# 
# 
# 
# library(plotly)
# 
# a <- sum_songs$album
# a_p <- sum_songs$a_label
# s_n <- sum_songs$s_label
# b <- sum_songs$sets
# c<- sum_songs$num_song
# 
# 
# plot_ly(sum_songs, ids = ~, labels = ~b, parents = ~a, type = 'sunburst')
# 
# 
# l<- rbind(unique(cbind(a,a_p)),cbind(b,s_n))
# 
# p <- c(rep("",length(unique(a))),a)
# 
# num_albums <- unique(cbind(sum_songs$album,sum_songs$n_album))[,2]
# 
# v <- as.numeric(c(num_albums,sum_songs$num_song))
# 
# i <- c(unique(a),paste(a,"-",b,sep = ""))
# 
# a <- as.data.frame(c(l[1:6],p[7:length(p)]))
# colnames(a) <- "album"
# 
# c <- left_join(a,albumcolors,by="album")
# 
# h <- paste("<b>",l,"</b><br>",ifelse(l %in% a,paste(a_p*100,"%<br>",a_n," times"),""))
# 
# argh <- unique(as.data.frame(cbind(l,p,v,i,c)))
# 
# argh <- arrange(argh,desc(v))
# 
# fig1 <- plot_ly(argh)
# 
# fig1 <- fig1 %>% add_trace(
#   labels = ~a,
#   parents = ~p,
#   values = ~v,
#   type = 'sunburst',
#   insidetextorientation='radial',
#   branchvalues='total',
#   hovertemplate = ~a_p
# ) %>%
#   layout(
#     extendsunburstcolors = TRUE,
#     colorway = ~col)
# 
# fig1
# 
# par(mfrow=c(1,1))
# 
# 
# 
# df = read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/coffee-flavors.csv')
# 
# fig <- plot_ly()
# 
# fig <- plot_ly(
#   type='sunburst',
#   ids=df$ids,
#   labels=df$labels,
#   parents=df$parents,
#   maxdepth=3,
#   insidetextorientation='radial'
# )
# fig
# 
# 
# dfa <- data.frame(
#   MainGroup = c("A", "A","A"),
#   Currency = c("USDollars","EUR","GBP"),
#   Percent = c(90,5,5),
#   Category = c("USD","Other","Other"),
#   stringsAsFactors = FALSE)    
# 
# plot_ly(labels = c(" ", "USD","Other",dfa$Currency), parents = c(""," "," ",dfa$Category), values = c(100,90,10,dfa$Percent), type = 'sunburst',
#         branchvalues='total')
# 
# 
# 
# fig <- plot_ly(
#   labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
#   parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
#   values = c(65, 14, 12, 10, 2, 6, 6, 4, 4),
#   type = 'sunburst',
#   branchvalues = 'total'
# )
# 
# fig
# 
# 
# 
# plot_ly(data = as.data.frame(argh), ids = ~i, labels= ~l, parents = ~p, values= ~v, type='sunburst')
# 
# 
# 
# 
# 
# 
# 

library(igraph)

base_colors <- colors()

# Filter the colors that have 1, 2, 3, or 4 at the end
colors_with_numbers <- base_colors[grepl("[1-4]$", base_colors)]

colors_with_numbers_no_g <- colors_with_numbers[!grepl("gray", colors_with_numbers) & !grepl("grey", colors_with_numbers)]

# Print the result
colors_with_numbers_no_g

# Remove any numbers 1:4 at the end of the color names
base_colors_no_numbers <- gsub("[1-4]$", "", colors_with_numbers_no_g)

# Remove duplicates (as numbers 1:4 create duplicates of base colors)
base_colors_no_numbers <- unique(base_colors_no_numbers)

songlist <- vwc[,c("city_num","sets","song_num")]



song_pos_avg <- songlist %>%
  select(
    sets, song_num
  ) %>%
  group_by(
    sets
  ) %>%
  summarize(
    avg_pos = mean(song_num)
  )


shows <- c("New York 1","Minneapolis 1", "Bend")


get_dag <- function(shows,songlist){
  
  setlists_1 <- songlist[which(songlist$city_num %in% shows),]
  
  setlists_1 <- setlists_1 %>%
    group_by(city_num) %>%
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
  
  edges <- data.frame(From = NA, To = setlists_1$sets[1], Show = setlists_1$city_num[1])
  
  for(i in 2:(nrow(setlists_1)-1)){
    edges <- rbind(edges,data.frame(From = setlists_1$sets[i-1], To = setlists_1$sets[i], Show = setlists_1$city_num[i]))
  }
  
  for(show in shows){
    edges <- edges[-min(which(edges$Show == show)),]
  }
  
  edges <- na.omit(edges)
  
  
  starting_songs <- setlists_1$sets[setlists_1$song_num==1]
  
  graph <- graph_from_data_frame(edges, directed = TRUE)
  
  vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)
  
  layout <- layout_as_tree(graph) 
  
  
  y_positions <- -song_pos_avg$avg_pos[match(V(graph)$name, song_pos_avg$sets)]
  
  layout <- cbind(layout[, 1], y_positions)
  
  
  if(length(shows) == 2){
  x_positions <- as.vector(ifelse(sapply(V(graph)$name, function(x) sum(setlists_1$sets == x)) > 1,0,ifelse(V(graph)$name %in% setlists_1$sets[which(setlists_1$city_num == shows[1])],-1,1)))
  
  layout <- cbind(x_positions,layout[,2])
  
  }
  
  if(length(shows) == 3){
    x_positions <- ifelse(V(graph)$name %in% setlists_1$sets[setlists_1$city_num == shows[1]], 0, ifelse(V(graph)$name %in% setlists_1$sets[setlists_1$city_num == shows[2]], -1,1))
    
    layout <- cbind(x_positions,layout[,2])
    
  }
  show_colors <- setNames(sample(rainbow(length(shows))), sort(unique(setlists_1$city_num)))
  
  
  edge_colors <- show_colors[edges$Show]
  
  plot(
    graph,
    layout = layout,
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
  legend("bottomright",bty = "n",
         legend=unique(names(edge_colors)),
         fill=unique(edge_colors), border=NA,
         cex = 0.7)
  
}





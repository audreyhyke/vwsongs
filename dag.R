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

setlists <- lapply(song_show_wide, function(col) {
  col[!is.null(col) & !is.na(col) & !(col %in% c(unique(vwd$sets[vwd$five == 1])))] # Remove NULL and NA values
})

setlists_f <- setlists[!names(setlists) %in% c("Saint Charles",
                                               "Austin 1", 
                                               "New Orleans", 
                                               "Salt Lake City",
                                               "Luton",
                                               "Barcelona")]

save(setlists_f, file = "~/vwsongs/vwshiny/data/setlistsf.RData")

setlists_1 <- setlists_f[28]

#####

song_positions <- do.call(rbind, lapply(setlists_1, function(setlist) {
  data.frame(Song = setlist, Position = seq_along(setlist))
}))

# Step 2: Calculate the average position for each song
avg_positions <- aggregate(Position ~ Song, data = song_positions, mean)
rownames(avg_positions) <- avg_positions$Song 


edges <- unlist(lapply(setlists_1, function(setlist) {
  if (length(setlist) > 1) {
    # Create pairs of consecutive songs
    transitions <- cbind(setlist[-length(setlist)], setlist[-1])
    return(as.vector(t(transitions))) # Flatten transitions into a vector
  }
}), use.names = FALSE)

# Step 2: Create a directed graph from the edges
#library(igraph)
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)


layout <- layout_nicely(graph)  # Start with a default layout
y_positions <- -avg_positions[V(graph)$name, "Position"]

layout <- cbind(layout[, 1], y_positions)

# Step 6: Plot the graph
plot(
  graph,
  layout = layout,
  vertex.label.color = "black",
  vertex.color = "pink",
  edge.arrow.size = 0.1,
  edge.color = "darkgrey",
  vertex.shape = "none",
  vertex.label.cex=.6,
  edge.curved=.1,
  main = "Setlist Directed Graph with Average Positions"
)



edges_with_shows <- do.call(rbind, lapply(names(setlists_1), function(show) {
  setlist <- setlists[[show]]
  if (length(setlist) > 1) {
    # Create pairs of consecutive songs and add the show name
    transitions <- data.frame(
      From = setlist[-length(setlist)],
      To = setlist[-1],
      Show = show
    )
    return(transitions)
  }
}))


starting_songs <- unique(sapply(setlists_1, function(setlist) setlist[1]))

# Step 2: Create a directed graph from the edges
graph <- graph_from_data_frame(edges_with_shows, directed = TRUE)

# Step 3: Assign unique colors for each show
show_colors <- setNames(sample(rainbow(length(setlists))), names(setlists))  # Generate unique colors


edge_colors <- show_colors[edges_with_shows$Show]

vertex_text_colors <- ifelse(V(graph)$name %in% starting_songs, "red", "black")

layout <- layout_as_tree(graph)  # Start with a default layout
y_positions <- -avg_positions[V(graph)$name, "Position"]

layout <- cbind(layout[, 1], y_positions)

# Step 6: Plot the graph
plot(
  graph,
  layout = layout,
  vertex.label.color = vertex_text_colors,
  vertex.color = "pink",
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.shape = "none",
  vertex.label.cex=.6,
  edge.curved=.1,
  main = "Setlist Directed Graph with Average Positions"
)


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

# Print the result
base_colors_no_numbers

random_base_colors <- sample(base_colors_no_numbers, length(setlists_1), replace = FALSE)





get_edge_colors <- function(show, num_edges,base_color) {
  # Generate a rainbow color gradient, from light to dark
  rainbow_palette <- colorRampPalette(c(paste(base_color,"1",sep = ""), paste(base_color,"4",sep = "")))(num_edges)
  return(rainbow_palette)
}



edge_colors <- unlist(lapply(names(setlists_1), function(show) {
  setlist <- setlists_1[[show]]
  transitions <- which(edges_with_shows$Show == show)  # Identify edges for this show
  base_color <- random_base_colors[which(names(setlists_1) == show)]  # Get the unique base color for this show
  edge_colors_for_show <- get_edge_colors(show, length(transitions), base_color)  # Get gradient colors for these edges
  return(edge_colors_for_show)
}))


plot(
  graph,
  layout = layout,
  vertex.label.color = vertex_text_colors,
  vertex.color = "pink",
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.shape = "none",
  vertex.label.cex=.6,
  edge.curved=.1,
  main = "Setlist Directed Graph with Average Positions"
)


#####
setlists_1 <- setlists_f[names(setlists_f) %in% c()]

song_positions <- do.call(rbind, lapply(setlists_1, function(setlist) {
  data.frame(Song = setlist, Position = seq_along(setlist))
}))

# Step 2: Calculate the average position for each song
avg_positions <- aggregate(Position ~ Song, data = song_positions, mean)
rownames(avg_positions) <- avg_positions$Song 


edges <- unlist(lapply(setlists_1, function(setlist) {
  if (length(setlist) > 1) {
    # Create pairs of consecutive songs
    transitions <- cbind(setlist[-length(setlist)], setlist[-1])
    return(as.vector(t(transitions))) # Flatten transitions into a vector
  }
}), use.names = FALSE)


edges_with_shows <- do.call(rbind, lapply(names(setlists_1), function(show) {
  setlist <- setlists[[show]]
  if (length(setlist) > 1) {
    # Create pairs of consecutive songs and add the show name
    transitions <- data.frame(
      From = setlist[-length(setlist)],
      To = setlist[-1],
      Show = show
    )
    return(transitions)
  }
}))

starting_songs <- unique(sapply(setlists_1, function(setlist) setlist[1]))

# Step 2: Create a directed graph from the edges
graph <- graph_from_data_frame(edges_with_shows, directed = TRUE)

vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)

layout <- layout_as_tree(graph)  # Start with a default layout
y_positions <- -avg_positions[V(graph)$name, "Position"]

layout <- cbind(layout[, 1], y_positions)

random_base_colors <- sample(base_colors_no_numbers, length(setlists_1), replace = FALSE)

edge_colors <- unlist(lapply(names(setlists_1), function(show) {
  setlist <- setlists_1[[show]]
  transitions <- which(edges_with_shows$Show == show)  # Identify edges for this show
  base_color <- random_base_colors[which(names(setlists_1) == show)]  # Get the unique base color for this show
  edge_colors_for_show <- get_edge_colors(show, length(transitions), base_color)  # Get gradient colors for these edges
  return(edge_colors_for_show)
}))

plot(
  graph,
  layout = layout,
  vertex.label.font = vertex_font_weight,
  vertex.label.color = "black",
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.shape = "none",
  vertex.label.cex=.7,
  edge.curved=.2,
  main = "Setlist Directed Graph with Average Positions"
)


show_colors <- setNames(sample(rainbow(length(setlists_1))), names(setlists_1))  # Generate unique colors


edge_colors <- show_colors[edges_with_shows$Show]

plot(
  graph,
  layout = layout,
  vertex.label.font = vertex_font_weight,
  vertex.label.color = "black",
  edge.arrow.size = 0.1,
  edge.color = edge_colors,
  vertex.shape = "none",
  vertex.label.cex=.7,
  edge.curved=.2,
  main = "Setlist Directed Graph with Average Positions"
)

#####

load("data/setlistsf.RData")

get_dag <- function(shows,setlists){
  
  setlists_1 <- setlists[names(setlists) %in% shows]
  
  song_positions <- do.call(rbind, lapply(setlists_1, function(setlist) {
    data.frame(Song = setlist, Position = seq_along(setlist))
  }))
  
  avg_positions <- aggregate(Position ~ Song, data = song_positions, mean)
  rownames(avg_positions) <- avg_positions$Song 
  
  
  edges <- unlist(lapply(setlists_1, function(setlist) {
    if (length(setlist) > 1) {
      transitions <- cbind(setlist[-length(setlist)], setlist[-1])
      return(as.vector(t(transitions))) 
    }
  }), use.names = FALSE)
  
  
  edges_with_shows <- do.call(rbind, lapply(names(setlists_1), function(show) {
    setlist <- setlists[[show]]
    if (length(setlist) > 1) {
      transitions <- data.frame(
        From = setlist[-length(setlist)],
        To = setlist[-1],
        Show = show
      )
      return(transitions)
    }
  }))
  
  starting_songs <- unique(sapply(setlists_1, function(setlist) setlist[1]))
  
  graph <- graph_from_data_frame(edges_with_shows, directed = TRUE)
  
  vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)
  
  layout <- layout_as_tree(graph) 
  y_positions <- -avg_positions[V(graph)$name, "Position"]
  
  layout <- cbind(layout[, 1], y_positions)
  
  show_colors <- setNames(sample(rainbow(length(setlists_1))), names(setlists_1)) 
  
  
  edge_colors <- show_colors[edges_with_shows$Show]
  
  plot(
    graph,
    layout = layout,
    vertex.label.font = vertex_font_weight,
    vertex.label.color = "black",
    edge.arrow.size = 0.1,
    edge.color = edge_colors,
    vertex.shape = "none",
    vertex.label.cex=.65,
    edge.curved=.2,
    main = "Setlist Directed Graph",
    asp = 1.5
  )
  
  
}


get_dag(c("Amsterdam","Paris","London 4"),setlists_f)





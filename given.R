rm(list = ls())
library(tidyverse)
library(pheatmap)
library(stringr)


load("data/given.RData")
load("data/vwd.RData")


colnames(given) <- unique(vwd$sets)

rownames(given) <- colnames(given)


get_heatmap <- function(given){
  colnames(given) <- str_trunc(colnames(given),12)
  
  rownames(given) <- colnames(given)
pheatmap(given,
         color = colorRampPalette(c("white", "blue", "darkblue"))(50), # Gradient colors
         cluster_rows = TRUE,   # Enable row clustering
         cluster_cols = TRUE,   # Enable column clustering
         main = "Song Similarity Heatmap", # Add title
         display_numbers = TRUE, # Show similarity values
         number_color = "black") # Number color
}



percent_given <- function(given,song1,song2){
  return(given[which(colnames(given) == song1),which(colnames(given) == song2)])
}

all_percent_given <- function(given,song){
  return(cbind(rownames(given),round(given[,which(colnames(given) == song)],2)))
}

percent_given(given,"Connect","Sympathy")

percent_given(given,"Sympathy","Connect")

all_percent_given(given,"Sympathy")







library(tidyverse)
library(dendextend)

rm(list = ls())

songp <- as.data.frame(read_csv("data/songpos.csv"))

rownames(songp) <- songp[,1]

songp <- songp[,-1]

as.matrix(songp) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend


par(mar = c(0, 5, 0, 15))

dend <- dend %>%
  set("labels_col", value = c("#011A2E", "#A1C4E3", "#A1C4E3","#36536A","#36536A","#516F89","#516F89"), k=7) %>%
  set("branches_k_color", value = c("#011A2E", "#A1C4E3", "#86A8C5","#1C364C","#36536A","#6C8BA7","#516F89"), k = 7)

plot(rev(dend),horiz=TRUE, axes=FALSE)










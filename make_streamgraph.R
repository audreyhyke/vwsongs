rm(list = ls())
library(ggplot2)
library(ggstream)

load("data/album_per.RData")

get_streamgraph <- function(album_percentages){
ggplot(album_percentages, aes(x = num, y = percentage, fill = album)) +
  geom_stream( bw =0.6,color = 1, lwd = 0.3)+
  scale_fill_manual(values = c(unique(album_percentages$col)[1:4],unique(album_percentages$col)[6],unique(album_percentages$col)[5])) +
  theme_void()
}




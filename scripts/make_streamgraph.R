rm(list = ls())
library(ggplot2)
library(ggstream)

load("data/album_per.RData")

get_streamgraph <- function(album_percentages_nf){
  ggplot(album_percentages_nf, aes(x = date_num, y = percentage, fill = album)) +
    geom_stream( bw =0.6,color = 1, lwd = 0.3)+
    scale_fill_manual(values = album_col) +
    theme_void()
}


streamgraph_lineplot <- ggplot(album_percentages_nf, aes(x = date_num, y = percentage, group = album, col = album)) +
  geom_line(alpha = 0.25) +
  scale_color_manual(values = album_col)+
  geom_smooth(method = "loess",se=FALSE) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

streamgraph_fillplot <- get_streamgraph(album_percentages_nf)

save(streamgraph_lineplot, streamgraph_fillplot, file = "vwshiny/data/streamgraphplots.RData")


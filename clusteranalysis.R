rm(list = ls())
library(ggpubr)
library(factoextra)

load("data/ClusteringAuto.RData")
simmat <- read.csv("data/similaritymatrixdc.csv",check.names=FALSE)
rownames(simmat) <- colnames(simmat)

list_clusters <- function(simmat,song,clust,clustassign){
  return(names(clustassign[[clust]][which(clustassign[[clust]] == clustassign[[clust]][which(names(clustassign[[clust]]) == song)])]))
}

plot_clusters <- function(kmeans_result,simmat,clust){
  fviz_cluster(kmeans_results[clust][[1]], data = simmat, 
               ellipse.type = "convex", 
               ggtheme = theme_bw()
  )
}

list_clusters(simmat, "Step",10,clustassign)
plot_clusters(kmeans_results,simmat,10)

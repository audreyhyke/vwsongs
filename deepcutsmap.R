rm(list = ls())
library(tidyverse)
library(leaflet)

load("data/deepcutsmap.RData")



m <- leaflet(show_sets) %>% 
  addTiles()  %>% 
  setView( lat=36, lng=-44 , zoom=2) %>%
  addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken='4szQK8qDpn6FyJKGDjggzDvo9O9qRmgWRrKRtre2nWgqJ5PFPaBcztuNUFPPM2SW')) %>%
  addCircleMarkers(~lon, ~lat, 
                   fillColor = ~color,fillOpacity = 0.7, color="white", radius=6, stroke=T,
                   label = mytextdc,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  )

m 

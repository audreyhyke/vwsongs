rm(list = ls())
library(tidyverse)
library(leaflet)

load("data/overlapmap.RData")

m <- leaflet(showavgoco) %>% 
  addTiles()  %>% 
  setView( lat=36, lng=-44 , zoom=2) %>%
  addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken='4szQK8qDpn6FyJKGDjggzDvo9O9qRmgWRrKRtre2nWgqJ5PFPaBcztuNUFPPM2SW')) %>%
  addCircleMarkers(~lon, ~lat, 
                   fillColor = ~mypalette(showavgo), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~showavgo, opacity=1, title = "Overlap Percentage </br> Average: 0.57", position = "bottomright" )

m 

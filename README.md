# Vampire Weekend Setlist Analysis

An interactive exploration of Vampire Weekend's live setlists, built with R and Shiny using data from [setlist.fm](https://www.setlist.fm/).

**[Open the app → audreyhyke.shinyapps.io/vwshiny](https://audreyhyke.shinyapps.io/vwshiny/)**

*The app is hosted on shinyapps.io, so it may take a few seconds to wake up on first load.*

## What's in the app

- **Your Data:** browse the setlists and per-album song information
- **Song Appearance Analysis:** how many songs from each album were played over time (line and stream graphs), k-means clusters of songs, a heatmap, and a two-song comparison
- **Song Position Analysis:** a dendrogram of where songs tend to fall in a set, and a "next song" explorer
- **Maps:** show-to-show overlap and deep cuts by location

## About the project

Setlists were scraped from setlist.fm and cleaned in R. Analyses include set-to-set similarity, clustering, and song-order patterns. Charts are built with ggplot2, plotly, and related packages.

## Repository

```
vwshiny/   The Shiny app and the data it loads
scripts/   R code for scraping, analysis, and building the plots
data/      Data files produced and used by the scripts
archive/   Earlier versions of the app
docs/      An early browser-only version of the app
```

Setlist data comes from setlist.fm contributors and may contain errors. This is a fan project and is not affiliated with Vampire Weekend.

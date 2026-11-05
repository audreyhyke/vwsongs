# Vampire Weekend Setlist Analysis

An interactive exploration of Vampire Weekend's live setlists, built with R and Shiny using data from [setlist.fm](https://www.setlist.fm/).

**[Open the app → audreyhyke.shinyapps.io/vwshiny](https://audreyhyke.shinyapps.io/vwshiny/)**

(The app is hosted on shinyapps.io, so it may take a few seconds to wake up on first load.)

## What's in the app

- **Your Data:** browse the setlists and per-album song information
- **Song Appearance Analysis:** albums over time (line and stream graphs), song clusters (k-means), a heatmap, and a two-song comparison
- **Song Position Analysis:** dendrogram of where songs fall in a set, and a "next song" explorer
- **Maps:** show overlap and deep cuts by location

## Repository layout

```
vwshiny/     The deployed Shiny app (app.R, its data/ folder, images in www/)
scripts/     R scripts that scrape setlist.fm, run the analysis, and build the plots/data the app loads
data/        Intermediate and output data files used by the scripts
data/raw/    Older raw/intermediate scraped data
docs/        Early shinylive (browser-only) export served by GitHub Pages; no longer the main site
archive/     Old and draft versions of the app, kept for reference
```

## Working on this project

1. Open `vwsongs.Rproj` in RStudio. **Scripts expect the project root as the working directory**, because they use relative paths like `data/...` and `vwshiny/data/...`.
2. `scripts/scrape_setlistfm.R` is the main pipeline: it scrapes setlists and writes the `.RData`/`.csv` files into `vwshiny/data/` that the app reads. Other scripts in `scripts/` build individual plots (`ridges.r`, `make_streamgraph.R`, `dag.R`, `given.R`, ...).
3. Run the app locally with `shiny::runApp("vwshiny")`.
4. Packages used include `shiny`, `bslib`, `tidyverse`, `plotly`, `gt`, `DT`, `ggridges`, `ggstream`, `pheatmap`, `igraph`, and `rvest`.

## Deploying

The app is deployed with `rsconnect` to shinyapps.io from the `vwshiny/` folder (the account config is in `vwshiny/rsconnect/`).

```r
rsconnect::deployApp("vwshiny")
```

## Notes

- Data covers shows from the Only God Was Above Us era; see `scripts/scrape_setlistfm.R` for exactly what is scraped.
- Setlist data comes from setlist.fm contributors and may contain errors.

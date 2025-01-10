library(tidyverse)
library(rvest)
library(urltools)
library(purrr)
library(dplyr)

setwd("~/.")

get_pages <- function(link){

   print("Getting page count...")

   session <- read_html(link)

   links <- session %>%
      html_nodes(".hidden-print li:nth-child(9) a") %>%
      html_text()

   links <- links[[1]]
}

url_expander <- function(link, num) {

   print(paste("Building URL", num, "..."))

   artist_query <- param_get(urls = link,
                             parameter_names = "query")

   build_url <- paste0("https://www.setlist.fm/search?page=", num, "&query=", artist_query)
}

# .hidden-print li:nth-child(9) a
get_links <- function (link) {

   print(paste0("Scraping ", link, "..."))

   session <- read_html(link)

   links <- session %>%
      html_nodes(".setlistPreview h2 a") %>%
      html_attr("href") %>%
      as.tibble()

   links <- links %>%
      mutate(link = paste0("https://www.setlist.fm/", value)) %>%
      select(-value)
}

get_songs <- function(link) {

   session <- read_html(link)
   
   tour <- session %>%
     html_node(".infoContainer p span span") %>%
     html_text()
   
   year <- session %>%
     html_node(".year") %>%
     html_text()

   sets <- session %>%
      html_nodes(".songLabel") %>%
      html_text()

   artist <- session %>%
      html_node("strong span") %>%
      html_text()

   venue <- session %>%
      html_node(".setlistHeadline span span span") %>%
      html_text()

   month <- session %>%
      html_node(".month") %>%
      html_text()

   day <- session %>%
      html_node(".day") %>%
      html_text()

   
   if(tour = "Only God Was Above Us"){
   print(paste("Getting", venue, year, "set lists..."))

   set_list <- list(sets = sets, artist = artist, venue = venue, month = month, day = day, year = year, tour = tour)
   }
}

scrape_artist <- function (artist_name, link) {

   # first get the number of pages we'll need to crawl from setlist.fm
   num <- 7
   # make a sequence to pass to map2 to build the URLs to scrape
   nums <- seq(1, num, 1)
   # repeat the base URL num number of times because map2 expectes vectors of equal length
   urls <- rep(link, times = num)
   # pass the 2 vectors to the url_explander function that will give us our list of
   # valid URLs of artist gigs to scrape
   artist_gigs <- map2(urls, nums, url_expander)
   # now that we have the base page URLs we need to get the links to the actual
   # set lists. We use the get_links function for that
   set_list_links <- map(artist_gigs, get_links)
   # I like to covert to a tibble here for ease of troubleshooting
   set_lists_df <- set_list_links %>% map_df(as_tibble)
   # we go right back to a vector now
   set_list_vector <- set_lists_df %>% pull(link)
   # grab set lists songs and map them to a dataframe
   set_lists <- map(set_list_vector, get_songs)
   set_lists_final <- set_lists %>% map_df(as_tibble)
   # clean up the resulting data frame and add song numbers, which is what we're interested in
   set_lists_final <- set_lists_final %>%
      group_by(artist, venue, month, day, year) %>%
      mutate(song_num = row_number())
   ### write the scrape to disk
   write_csv(set_lists_final, paste0(artist_name, "_set_lists.csv"))
}




scrape_artist("vw", "https://www.setlist.fm/search?query=vampire+weekend")


vw <- read.csv(file = "vw_set_listsCC.csv")

vw[1528,1] <- "Sympathy"

ndny <- c("New Dorp. New York")

names(ndny) <- "sets"

vw<- rbind(vw,unlist(c(ndny,vw[1528,-1])))

vw <- vw %>%
  filter(tour == "Only God Was Above Us")

vw[3,1] <- "One (Blake's Got a New Face)"


albums <- read.csv("~/Downloads/vwalbums - Sheet1-5.csv",header = F)

names(albums) <- c("sets", "album")


### cocaine cowboys???

vw <- vw %>%
  left_join(albums, by = "sets")

vwna <- na.omit(vw)

city <- NA

for(st in vwna[,3]){
  city <- c(city,paste(c(strsplit(strsplit(st, ",")[[1]][2],"")[[1]][-1]), collapse = ""))
}

city <- city[-1]

vwna <- cbind(vwna,city)
write_csv(vwna,"vw_setlist_na_city.csv")


vwc <- read_csv("data/vw_setlist_na_city.csv")


write_csv(vwna, file = "data/vw_setlist_na.csv")

songs <- vwd[,1]

songcount <- as.data.frame(table(songs))

deepcuts <- songcount[songcount$Freq == 1,1]
fivecuts <- songcount[songcount$Freq <= 5,1]

deepcutslist <- vwd %>%
  filter(
    sets %in% deepcuts
  )

fivecutslist <- vwd %>%
  filter(
    sets %in% fivecuts
  )

write_csv(deepcutslist, file = "data/deepcutslist.csv")




vwna <- vwna %>%
  mutate(
  venue2 = paste0(venue," ", day)
)

songlist <- vwna %>%
  select(
    sets,venue2
  ) %>%
  pivot_wider(
  names_from = venue2,
  values_from = sets
) %>%
  rev()


cross <- c(unlist(songlist[[2]]),unlist(songlist[[1]]))

(length(cross) - length(unique(cross)))/length(unique(cross))


overlap <- matrix(nrow = 58, ncol = 58)

for(i in 1:58){
  for(j in 1:58){
    cross <- c(unlist(songlist[[59-i]]),unlist(songlist[[59-j]]))
    overlap[i,j] <- (length(cross) - length(unique(cross)))/length(unique(cross))
  }
}




deepcutslist <- vwc %>%
  filter(
    sets %in% deepcuts
  )




songlist <- vwc %>%
  select(
    sets,city
  ) %>%
  pivot_wider(
    names_from = city,
    values_from = sets
  )



overlap <- matrix(nrow = 58, ncol = 58)

for(i in 1:58){
  for(j in 1:58){
    cross <- c(unlist(songlist[[59-i]]),unlist(songlist[[59-j]]))
    overlap[i,j] <- (length(cross) - length(unique(cross)))/length(unique(cross))
  }
}


rownames(overlap) <- rev(unique(vw$city))
colnames(overlap) <- rev(unique(vw$city))

write_csv(as.data.frame(overlap), file = "vwoverlap.csv")


#no fests

vwf <- vwc %>%
  filter(
    !(city %in% c("Saint Charles",
                 "Austin 1", 
                 "New Orleans", 
                 "Salt Lake City",
                 "Luton",
                 "Barcelona")),
      !(month %in% c("Apr"))
  )

vwf$date1 <- paste(vwf$month,vwf$day)

vwf <- left_join(vwf,vwr1, by = c("date1" = "date1"))

cityf <- as.data.frame(unique(vwf$city.y))
colnames(cityf) <- "city"

write_csv(cityf, file = "nofestcityvw.csv")

cap <- read_csv("vwcap.csv", col_names = TRUE)

vwcap <- vwf %>%
  left_join(cap, by = c("city.y" = "city"))

# rarity score... avg overlap?

vwdc <- vwcap %>%
  mutate(
    once = ifelse(sets %in% deepcutslist$sets,1,0),
    five = ifelse(sets %in% songcount[songcount$Freq <= 5,1],1,0)
  )

unique(vwdc$sets[vwdc$five == 1])

vwdc$sets[vwdc$once == 1]

vwdc <- vwdc %>%
  filter(
    sets != "Paper Planes / Baba O'Riley"
  )

vwdc[vwdc$once == 1,c("sets","city.y")]

vwc1 <- vwdc %>%
  mutate(
    city1 = ifelse(grepl("[0-9]" , vwdc$city.y), substr(vwdc$city.y, 1, nchar(vwdc$city.y) - 2), vwdc$city.y)
  )

library(ggmap)



venues <- as.data.frame(unique(vwc1$venue))

names(venues) <- "venue"

coords <- venues %>%
  mutate(
    coords = geocode(venue)
  )

co <- cbind(venues$venue,as.data.frame(coords$coords))

names(co) <- c("venue", "lon", "lat")


vwco <- vwc1 %>%
  left_join(co, by = c("venue" = "venue"))


vws <- vwco %>%
  mutate(
    state = ifelse(length(strsplit(venue, split = ", ")[[1]]) == 3, "NA", strsplit(venue, split = ", ")[[1]][3])
  )

## NO FESTIVALS!!!

songlistf <- vws %>%
  select(
    sets,city.y
  ) %>%
  pivot_wider(
    names_from = city.y,
    values_from = sets
  )

overlapf <- matrix(nrow = 52, ncol = 52)

for(i in 1:52){
  for(j in 1:52){
    cross <- c(unlist(songlistf[[i]]),unlist(songlistf[[j]]))
    overlapf[i,j] <- (length(cross) - length(unique(cross)))/length(unique(cross))
  }
}

rownames(overlapf) <- unique(vws$city.y)
colnames(overlapf) <- unique(vws$city.y)

overlaplist <- as.data.frame(unlist(as.data.frame(overlapf)))
names(overlaplist) <- "overlap"

overlaplist <- overlaplist[overlaplist$overlap <1,]

avgoverlap <- round(mean(overlaplist),2)

overlapff <- ifelse(overlapf >= 1, NA, overlapf)

showavgo <- cbind(unique(vws$city.y),as.data.frame(round(colMeans(overlapff, na.rm = T),2)),avgoverlap)

names(showavgo) <- c("city", "showavgo", "totalavgo")

vwcv <- as.data.frame(cbind(vws$city.y,vws$venue,vws$date1))
names(vwcv) <- c("city", "venue","date")

showavgo <- showavgo %>%
  left_join(unique(vwcv),by = c("city" = "city"))

showavgoco <- showavgo %>%
  left_join(co, by = c("venue.x" = "venue"))
  

vwr <- vws %>%
  left_join(showavgo,by = c("city.y" = "city"))

vwr <- vwr %>%
  mutate(
    totalavgo <- avgoverlap
  )

library(ggplot2)
library(sf)
library(leaflet)

mytext <- paste(
  "Overlap: ", showavgoco$showavgo, "<br/>", 
  "Show: ", showavgoco$city.y, "<br/>", 
  "Date: ", showavgoco$date1, ", 2024", sep="") %>%
  lapply(htmltools::HTML)

# Final Map

# jittering

j = 0.1
df <- showavgoco %>% 
  mutate(
    lon = ifelse(grepl("[2]" , city),lon + 0.1,lon),
    lon = ifelse(city == "London 3", lon - 0.1, lon)
  )

mybins <- quantile(showavgoco$showavgo,seq(0,1,by=0.2))
mypalette <- colorBin( palette="RdBu", domain=showavgoco$showavgo, na.color="transparent", bins=mybins, reverse = T)

m <- leaflet(df) %>% 
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





leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
  addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken='4szQK8qDpn6FyJKGDjggzDvo9O9qRmgWRrKRtre2nWgqJ5PFPaBcztuNUFPPM2SW'))

####### SAVE

save(showavgoco,mybins,mypalette,mytext, file = "data/overlapmap.RData")

write_csv(vwd,file = "vwallcoords.csv")
write_csv(showavgoco, "showavg.csv")

vwd <- read_csv("data/vwallcoords.csv")
showavgoco <- unique(vwr[c("venue.x","city","month","day","lon","lat","showavgo")])
showavgoco <- cbind(showavgoco,round(mean(showavgoco$showavgo),2))


#### NEW EACH SHOW

# length(unique(vwr$sets))  54

numVW <- 11
numC <- 10
numM <- 12
numF <- 18
numOG <- 10


vwd <- vwd %>%
  mutate(
    date = as.Date(paste(month," ",day, " 24"),format = "%b %d %y")
  )

vwd<- vwd %>%
  left_join(albumcolors,by = "album")


citydate$num <- rev(1:52)

album_percentages <- vwd %>%
  group_by(city.y, album,col) %>% 
  summarize(count = n(), .groups = "drop") %>% # Count songs per album per city
  group_by(city.y) %>% 
  mutate(percentage = (count / sum(count)))

citydate <- unique(vwd[,c("city.y","date")])

citydate$num <- rev(seq(1,52))

album_percentages <- album_percentages %>%
  left_join(citydate, by = c("city.y" = "city.y"))

save(album_percentages,file = "data/album_per.RData")


album_percentages %>% 
  ggplot(aes(x = num, y = percentage, fill = album)) +
  geom_area()



ggplot(album_percentages, aes(num, fill = album)) +
  geom_density(position = "fill")+
  scale_fill_manual(values = album_percentages$col)

ggplot(album_percentages, aes(x = num, y = percentage, fill = album)) +
  geom_bar(stat = "identity", position = "stack") +  # Bar plot reflecting percentages
  scale_fill_manual(values = album_percentages$col)

print(vwd %>%
  filter(
    city.y == "Austin 2"
  ) %>%
  select(
    sets
  ), n = 27)
  
  
  
  %>%
  write_csv(file = "singleshowvw.csv")

library(ggstream)



write_csv(album_percentages,file = "albumper.csv")

ccol <- "#583951"
vwcol <- "#F09F49"
ogcol <- "#546C98"
fcol <- "#479A53"
mcol <- "#828282"
otcol <- "#E96586"

albumcolors <- as.data.frame(cbind(c("Contra","Vampire Weekend","Only God Was Above Us","Father of the Bride","Modern Vampires of the City","Other"),c(ccol,vwcol,ogcol,fcol,mcol,otcol)))

colnames(albumcolors) <- c("album","col")

save(albumcolors, file = "data/albumcolors.RData")

album_percentages$album <- factor(album_percentages$album, levels=rev(c("Only God Was Above Us", "Vampire Weekend", "Father of the Bride", "Modern Vampires of the City", "Contra", "Other")))

ggplot(album_percentages, aes(x = num, y = count, fill = album)) +
  geom_stream(type = "ridge", bw =0.6,color = 1, lwd = 0.3)+
  scale_fill_manual(values = c(otcol,ccol,mcol,fcol,vwcol,ogcol)) +
  theme_void()

vws$date1 <- paste(vws$month,vws$day)
vwr$date1 <- paste(vwr$month,vwr$day)


vwr1 <- vwr[,c("date1","city.y")]
vwr1 <- unique(vwr1)

vwsc <- left_join(vws,vwr1, by = c("date1" = "date1"))

vws <- vwsc


cities <- unique(vwr$city)

first_show <- vwd %>%
  group_by(sets) %>%
  arrange(date) %>% # Ensure data is sorted by date
  slice(1) %>% # Select the first occurrence of each song
  select(sets, first_show = city.y, date) %>%
  arrange(date)

write_csv(first_show, file = "firstshowdate.csv")

deepcutsco <- vwd %>%
  filter(
    five == 1
  ) %>%
  select(
    sets, date, once, five, lon, lat, city.y
  ) %>%
  mutate(
    sets = ifelse(once, paste("*",sets),sets),
    #color = ifelse(once, "gold","slategrey")
  )


dcc <- unique(deepcutsco[,-c(1,3,4)])

show_sets <- deepcutsco %>%
  group_by(date, city.y,lon,lat) %>%
  summarize(songs = list(sets), .groups = "drop")

show_sets$songs <- sapply(show_sets$songs, function(x) paste(x, collapse = ", "))

show_sets$color <- ifelse(grepl("[*]" , show_sets$songs),"gold","slategrey")


mytextdc <- paste(
  "Songs: ", show_sets$songs, "</br>",
  "Show: ", show_sets$city.y, "<br/>", 
  "Date: ", show_sets$date, sep="") %>%
  lapply(htmltools::HTML)

# Final Map

# jittering

j = 0.1
show_sets <- show_sets %>% 
  mutate(
    lon = ifelse(grepl("[2]" , city.y),lon + 0.1,lon),
    lon = ifelse(city.y == "London 3", lon - 0.1, lon)
  )

mybins <- quantile(showavgoco$showavgo,seq(0,1,by=0.2))
mypalette <- colorBin( palette="RdBu", domain=showavgoco$showavgo, na.color="transparent", bins=mybins, reverse = T)

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
save(deepcuts,fivecuts,deepcutslist,fivecutslist,show_sets, mytextdc, file = "data/deepcutsmap.RData")


#### beginning or ending track?

songpos3 <- vwd %>%
  filter(
    !five
  ) %>%
  select(
    sets,city.y,song_num
  ) %>%
  pivot_wider(
    names_from = sets,
    values_from = song_num
  ) %>% 
  t()


songpos2 <- vwd %>%
  group_by(city.y) %>%
  summarize(
    songpos = list(sets)
  )

songpos <- as.data.frame(cbind(unlist(songpos2[1,2][[1]]),seq(1,length(unlist(songpos2[1,2][[1]])))/length(unlist(songpos2[1,2][[1]])),songpos2$city.y[1]))

names(songpos) <- c("song","pos","city")

for(i in 2:52){
  l <- unlist(songpos2[i,2][[1]])
  r <- as.data.frame(cbind(l,seq(1,length(l))/length(l),songpos2$city.y[i]))
  names(r) <- c("song","pos","city")
  songpos <- rbind(songpos,r)
  
}

songpos <- songpos %>% pivot_wider(names_from = city, values_from = pos) %>% as.data.frame()
songpos <- songpos[,-1]
songpos <- songpos %>% as.data.frame()
songpos <- unname(songpos)

songpos <- songpos3[-1,]

songpos <- as.data.frame(matrix(,38,52))
for(i in 1:dim(songpos)[1]){
  for(j in 1:dim(songpos)[2]){
    if(length(unlist(songpos1[i,j][[1]]))==0){
      songpos[i,j] <- mean(as.numeric(unlist(songpos1[i,])),na.rm = T)
    }
    else if(length(songpos1[i,j][[1]])>1){
      songpos[i,j] <- mean(songpos1[i,j][[1]])
    }
    else(songpos[i,j] <- songpos1[i,j][[1]])
  }
}
songpos <- as.data.frame(songpos)
rownames(songpos) <- rownames(songpos1)

write_csv(cbind(rownames(songpos),songpos), file = "data/songpos.csv")

#songpos1 <- songpos1 %>%
#  mutate(across(everything(), ~ ifelse(is.na(.), rowMeans(songpos1, na.rm = TRUE), .)))


songpos1 <- as.data.frame(songpos3)

rownames(songpos1) <- rownames(songpos3)

songpos1 <- songpos1[-1,]
songpos1 <- songpos1[-43,] # flower moon?
songpos1 <- songpos1[-c(1,2,7),]
songpos1 <- songpos1[-c(30),]
# songpos1[24,] <- rep(40,52)
# songpos1[25,] <- rep(40,52)
# songpos1[34,] <- rep(40,52)

songposc <- songpos

songpos[c(21,22,30),] <- rep(40,52)
library(dendextend)

par(mar = c(0, 0, 0, 20))

as.matrix(songpos1) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend




dend <- dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey","red","yellow","blue","green"), k=7) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey","red","yellow","blue","green"), k = 7)

dendr <- reorder(dend,-svd(t(songpos1))$v[,1])

  plot(rev(dend),horiz=TRUE, axes=FALSE)

write_csv(songpos,"songpos.csv")

songpos1 <- read_csv("data/songpos.csv")


graphsp <- songpos3[-c(44,1),]



gsp <- as.data.frame(matrix(,42,52))
for(i in 1:dim(graphsp)[1]){
  for(j in 1:dim(graphsp)[2]){
    if(length(unlist(graphsp[i,j][[1]]))==0){
      gsp[i,j] <- NA
    }
    else if(length(graphsp[i,j][[1]])>1){
      gsp[i,j] <- mean(graphsp[i,j][[1]])
    }
    else(gsp[i,j] <- graphsp[i,j][[1]])
  }
}

rownames(gsp) <- rownames(graphsp)

library(matrixStats)

gsp <- as.matrix(gsp)

avgsonpos <- rowMeans(gsp,na.rm = T)
  
sdsonpos <- rowSds(gsp,na.rm=T)

# over 5: cousins, oc, unb, campus.

# gsp <- gsp[-c(1,2,7),]
# avgsonpos <- avgsonpos[-c(1,2,7),]


avgsonpos <- sort(avgsonpos)


for(i in 1:39){
  plot(1:52,unname(gsp[which(rownames(gsp) == names(avgsonpos)[i]),]), xlab = names(avgsonpos)[i],ylab = "position",ylim = c(0,45))
  Sys.sleep(.1)
}








order.dendrogram(dend)

plot(reorder(dend,38:1,agglo.FUN = mean))


x <- rnorm(10)
hc <- hclust(dist(x))
dd <- as.dendrogram(hc)
dd.reorder <- reorder(dd, 10:1)
plot(dd, main = "random dendrogram 'dd'")

plot(dd.reorder, main = "reorder(dd, 10:1)")
plot(reorder(dd, 10:1, agglo.FUN = mean), main = "reorder(dd, 10:1, mean)")

labels(dend)

svd(t(songpos1))$v[,1]


plot(reorder(dend,svd(t(songpos1))$v[,1]))



act1 <- c("Hol","HYN","MR","B","BLOC","OBGANF","ISC")
act2 <- c("ICP","Cl", "CCKK","Co","WS")
act3 <- c("St","TL","Ba","Su","M79","Sy","NDNY")
act4 <- c("TS","P","R","O","Ca","GUTG","Hor","GXC","DY")
act5 <- c("AP","PSG","CC","MB")
act6 <- c("HaHu","HaHa","Ho")
act7 <- c("W","WY","YH")


#### CLUSTER

showsong <- as.data.frame(unique(cbind(vwd$city.y,vwd$sets)))
showsong[,1] <- as.numeric(factor(showsong[,1]))

colnames(showsong) <- c("city_id","song")

incidence_matrix <- showsong %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = song, values_from = value)
incidence_matrix[is.na(incidence_matrix)] <- 0
incidence_matrix <- incidence_matrix[,-1]
# Remove the `show_id` column to focus on the song matrix
song_matrix <- incidence_matrix


for(i in dim(incidence_matrix)[1]){
  for(j in dim(incidence_matrix)[2]){
    song_matrix[i,j]<-as.numeric(incidence_matrix[i,j])
  }
}

song_matrix <- as.matrix(song_matrix)

# Calculate the co-occurrence matrix
co_occurrence_matrix <- t(song_matrix) %*% song_matrix

# Calculate the total number of shows each song was played in
song_totals <- diag(co_occurrence_matrix)

# Compute the Jaccard Index for each song pair
similarity_matrix <- co_occurrence_matrix / (outer(song_totals, song_totals, "+") - co_occurrence_matrix)

# Perform clustering
dist_matrix <- as.dist(1 - similarity_matrix)  # Convert similarity to dissimilarity
hc <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc, main = "Song Clustering by Co-Occurrence")

library(stats)
kmeans_result <- kmeans(similarity_matrix, centers = 7)  # Change '3' to desired number of clusters
cluster_assignments <- kmeans_result$cluster


library(stringr)
for(i in 1:56){
  if (nchar(rownames(similarity_matrix)[i]) >12){
    rownames(similarity_matrix)[i] <- str_trunc(rownames(similarity_matrix)[i], 12)
  }
}

colnames(similarity_matrix) <- rownames(similarity_matrix)

write_csv(as.data.frame(similarity_matrix),file = "similaritymatrix.csv")

similarity_matrix <- read_csv("similaritymatrix.csv")

library(pheatmap)
pheatmap(similarity_matrix,
         color = colorRampPalette(c("white", "blue", "darkblue"))(50), # Gradient colors
         cluster_rows = TRUE,   # Enable row clustering
         cluster_cols = TRUE,   # Enable column clustering
         main = "Song Similarity Heatmap", # Add title
         display_numbers = TRUE, # Show similarity values
         number_color = "black") # Number color


similarity_df <- as.data.frame(as.table(similarity_matrix))

# Rename the columns for clarity
colnames(similarity_df) <- c("Song1", "Song2", "Similarity")

# Load ggplot2
library(ggplot2)

# Create the heatmap using ggplot2
ggplot(similarity_df, aes(x = Song1, y = Song2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
        axis.text.y = element_text(angle = 0)) +
  labs(title = "Song Similarity Heatmap",
       fill = "Similarity") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())



P_X <- mean(song_matrix[,54])

# Calculate P(X ∩ Y) = probability that both SongX and SongY were played
P_X_and_Y <- mean(song_matrix[,54] & song_matrix[,53])

# Calculate P(Y|X) = conditional probability
P_Y_given_X <- P_X_and_Y / P_X

# Output the result
P_Y_given_X


given <- matrix(NA,56,56)

for(i in 1:dim(given)[1]){
  for(j in 1:dim(given)[2]){
    
    P_X <- mean(song_matrix[,i])
    
    # Calculate P(X ∩ Y) = probability that both SongX and SongY were played
    P_X_and_Y <- mean(song_matrix[,i] & song_matrix[,j])
    
    # Calculate P(Y|X) = conditional probability
    P_Y_given_X <- P_X_and_Y / P_X
    
    # Output the result
    given[i,j]<-P_Y_given_X
    
  }
}

colnames(given) <- colnames(similarity_matrix)
rownames(given) <- rownames(similarity_matrix)

write_csv(data.frame(given),file = "data/given.csv")

given <- as.data.frame(read_csv("data/given.csv"))

rownames(given)<- colnames(given)

pheatmap(given,
         color = colorRampPalette(c("white", "blue", "darkblue"))(50), # Gradient colors
         cluster_rows = TRUE,   # Enable row clustering
         cluster_cols = TRUE,   # Enable column clustering
         main = "Song Similarity Heatmap", # Add title
         display_numbers = TRUE, # Show similarity values
         number_color = "black") # Number color

percent_given <- function(given,song1,song2){
  return(given[which(colnames(given) == song1),which(colnames(given) == song2)])
}

all_percent_given <- function(given,song){
  return(cbind(rownames(given),given[,which(colnames(given) == song)]))
}

percent_given(given,"Connect","Sympathy")

percent_given(given,"Sympathy","Connect")

all_percent_given(given,"Sympathy")

#c <- t(as.data.frame(song_matrix[1][[1]]))

#c <- cbind(c,t(rep(NA,52-length(c))))



#for(i in 2:52){
#  c1 <- t(as.data.frame(song_matrix[i][[1]]))
#  if(length(c1) != 52){
#    c1 <- cbind(c1,t(rep(NA,52-length(c1))))
#  }
#  c <- rbind(c,c1)
#}

### cluster graph NO DEEP CUTS / SD > 5

library(ggpubr)
library(factoextra)

vwdcc <- vwd[-which(vwd$five==1 | vwd$sets %in% c("Flower Moon")),]

songposdc <- vwdcc %>%
  select(
    sets,city.y,song_num
  ) %>%
  pivot_wider(
    names_from = sets,
    values_from = song_num
  ) %>% 
  t()

showsongdc <- as.data.frame(unique(cbind(vwdcc$city.y,vwdcc$sets)))
showsongdc[,1] <- as.numeric(factor(showsongdc[,1]))

colnames(showsongdc) <- c("city_id","song")

incidence_matrix <- showsongdc %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = song, values_from = value)
incidence_matrix[is.na(incidence_matrix)] <- 0
incidence_matrix <- incidence_matrix[,-1]
# Remove the `show_id` column to focus on the song matrix
song_matrix <- incidence_matrix


for(i in dim(incidence_matrix)[1]){
  for(j in dim(incidence_matrix)[2]){
    song_matrix[i,j]<-as.numeric(incidence_matrix[i,j])
  }
}

song_matrix <- as.matrix(song_matrix)

# Calculate the co-occurrence matrix
co_occurrence_matrix <- t(song_matrix) %*% song_matrix

# Calculate the total number of shows each song was played in
song_totals <- diag(co_occurrence_matrix)

# Compute the Jaccard Index for each song pair
similarity_matrix <- co_occurrence_matrix / (outer(song_totals, song_totals, "+") - co_occurrence_matrix)

# Perform clustering
dist_matrix <- as.dist(1 - similarity_matrix)  # Convert similarity to dissimilarity
hc <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc, main = "Song Clustering by Co-Occurrence")

library(stats)


wss <- (nrow(similarity_matrix)-1)*sum(apply(similarity_matrix,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(similarity_matrix,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


kmeans_result <- kmeans(similarity_matrix, centers = 6)  # Change '3' to desired number of clusters
cluster_assignments <- kmeans_result$cluster

#write_csv(as.data.frame(similarity_matrix),"similaritymatrixdc.csv")



fviz_cluster(kmeans_result, data = similarity_matrix, 
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


plot_clusters <- function(kmeans_result,simmat,clust){
  fviz_cluster(kmeans_results[clust][[1]], data = simmat, 
               ellipse.type = "convex", 
               ggtheme = theme_bw()
  )
}

clustassign <- list()
kmeans_results <- list()
for(i in 1:10){
  kmeans_result <- kmeans(similarity_matrix, centers = i)  # Change '3' to desired number of clusters
clustassign<- c(clustassign,list(kmeans_result$cluster))
kmeans_results <- c(kmeans_results,list(kmeans_result))}

list_clusters <- function(simmat,song,clust,clustassign){
  
  return(names(clustassign[[clust]][which(clustassign[[clust]] == clustassign[[clust]][which(names(clustassign[[clust]]) == song)])]))
}

plot_clusters(kmeans_result,similarity_matrix,6)

list_clusters(similarity_matrix,"Campus",6,clustassign)

save(kmeans_results,clustassign, file = "ClusteringAuto.RData")


library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(similarity_matrix), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)


similaritymatrixdc <- as.data.frame(similaritymatrix)
rownames(similaritymatrixdc) <- colnames(similaritymatrixdc)

write.table(similaritymatrixdc,file = "similaritymatrixdc.csv",row.names = T)

other <- as.data.frame(unique(cbind(vwd$month,vwd$day,vwd$city.y)))

colnames(other) <- c("month","day","city.y")

other <- other %>%
  mutate(
    date1 = paste(other$month,other$day)
  )

new <- showavgoco %>%
  mutate(
    date1 = paste(showavgoco$month,showavgoco$day),
    totalavg = showavgoco$`round(mean(showavgoco$showavgo), 2)`
  )
showavgoco <- new %>%
  left_join(
    other, by = "date1"
  ) %>%
  select(
    date1,city.y,lon,lat,venue.x,showavgo, totalavg
  )

left_join(new,other, by = date1)


library(stringr)

### NEXT SONG CHANCE

vwd <- vwallcoords

song_show <- as.data.frame(cbind(vwd$sets,vwd$city.y,vwd$song_num))

save(song_show,file = "data/songshow.RData")

song_show <- as.data.frame(song_show)

colnames(song_show) <- c("sets","city.y","song_num")

song_show_wide <- song_show %>%
  pivot_wider(
    names_from = city.y,
    values_from = sets
  ) 

save(song_show_wide,file = "data/song_shows.RData")

song_show_wide <- song_show_wide[,-1]

song_show_NA <- as.data.frame(song_show_wide)

max_songs <- 41


# songs <- as.data.frame(song_show_wide)[,1] %>%
#   purrr::keep(~ length(.x) > 0) %>%
#   unlist()
# 
# song_show_NA <- c(songs,rep("no song, because it was the end of the show",max_songs - length(songs)))
# 
# 
# for(show in 2:ncol(song_show_wide)){
#   
#   songs <- as.data.frame(song_show_wide)[,show] %>%
#     purrr::keep(~ length(.x) > 0) %>%
#     unlist()
#   songs <- c(songs,rep("no song, because it was the end of the show",max_songs - length(songs)))
#   
#   song_show_NA <- cbind(song_show_NA,songs)
#   
#   
# }
# 
# song_show_NA <- as.data.frame(song_show_NA)
# 
# song_show_list <- unlist(song_show_NA)


song_show_N <- rbind(song_show_wide,rep("nothing, because it was the end of the show"))

song_show_N <- unname(unlist(song_show_N))

names_songs <- unique(vwd$sets)

song_next <- list(c(song_show_N[which(song_show_N == names_songs[1])+1]))

for(i in 2:(length(names_songs))){
  song_next <- append(song_next,list(c(song_show_N[which(song_show_N == names_songs[i])+1])))
}

names(song_next) <- names_songs

save(song_next,file = "data/nextsongs.RData")

x <- list("x")

append(x,list(table(song_next$Cousins) / length(song_next$Cousins)))


# song_next_per <- song_next[[1]] %>%
#   table() %>%
#   sort(decreasing = TRUE) %>%
#   as.data.frame() %>%
#   mutate(
#     Percentage = round(Freq / sum(Freq) * 100, 2),
#     Description = paste0(Var1, ": ", Percentage, " percent chance (", Freq, " out of ", sum(Freq), " times)")
#   ) %>%
#   pull(Description)


song_next_per <- list(paste(names(rev(sort(table(song_next[1])))),": ",round(rev(sort(table(song_next[1]))) / length(song_next[1][[1]]),2)*100," percent chance (",rev(sort(table(song_next[1])))," out of ",length(song_next[1][[1]])," times)",sep=""))

names(song_next_per) <- names(song_next[i])


for(i in 2:length(song_next)){
  
  next_per <- list(paste(names(rev(sort(table(song_next[i])))),": ",round(rev(sort(table(song_next[i]))) / length(song_next[i][[1]]),2)*100," percent chance (",rev(sort(table(song_next[i])))," out of ",length(song_next[i][[1]])," times)",sep=""))
  
  names(next_per) <- names(song_next[i])
  
  song_next_per <- append(song_next_per,next_per)
}


save(song_next,song_next_per,file = "data/nextsongs.RData")


setlist_albums_songs <- vwd %>%
  select(
    city.y,album,sets
  ) %>%
  left_join(albumcolors,by = "album")


left_join(setlist_albums_songs,albumcolors,by = "album")



save(setlist_albums,setlist_albums_songs,file = "data/setlistalbums.RData")


set_fest <- vwc %>%
  filter(
    (city %in% c("Saint Charles",
                  "Austin", 
                  "New Orleans", 
                  "Salt Lake City",
                  "Luton",
                  "Barcelona")),
    !(month %in% c("Oct"))
  )

set_fest$city.y <- ifelse(set_fest$city == "Austin","Austin 1",set_fest$city)

set_fest_cas <- set_fest %>%
  select(
    city.y,album,sets
  )

set_cas <- vwd %>%
  select(
    city.y,album,sets
  )

all_sets <- rbind(set_fest_cas,set_cas)


all_sets_col <- all_sets %>%
  left_join(albumcolors,by = "album")

setlist_albums_songs <- all_sets_col

save(setlist_albums_songs,file = "data/setlistalbums.RData")

song_show <- setlist_albums_songs %>%
  select(
    city.y,sets
  )


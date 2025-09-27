library(leaflet)
library(shiny)
library(ggpubr)
library(factoextra)
library(data.table)
library(stringr)
library(bslib)
library(ggplot2)
library(ggstream)
library(plotly)
library(tidyverse)
library(dplyr)
library(pheatmap)
library(igraph)
library(gt)
library(scales)
library(DT)

load("data/ClusteringAuto.RData")
load("data/nextsongs.RData")
load("data/album_per.RData")
load("data/setlistalbums.RData")
load("data/albumcolors.RData")
load("data/given.RData")
load("data/vwd.RData")
load("data/song_shows.RData")
load("data/overlapmap.RData")
load("data/deepcutsmap.RData")
load("data/setlistsf.RData")
load("data/vwc_pt2.RData")
albums <- read.csv("data/songalbums.csv")[,-1]
names(albums) <- c("song","album")

ccol <- "#583951"
vwcol <- "#F09F49"
ogcol <- "#546C98"
fcol <- "#479A53"
mcol <- "#828282"
otcol <- "#E96586"


album_col <- c("Vampire Weekend" = vwcol, "Contra" = ccol, "Only God Was Above Us" = ogcol, "Father of the Bride" = fcol, "Modern Vampires of the City" = mcol, "Other" = otcol)


song_col <- left_join(albums,albumcolors,by = "album")[,c(1,3)]

l_albumcolors <- as.data.frame(cbind(albumcolors$album,c("#d8d2e7","#f8e6d0","#d2e1f1","#dce9d5","#efefef","#edcdcd")))

names(l_albumcolors) <- c("album","col")

l_song_col <- left_join(albums,l_albumcolors, by = "album")[,c(1,3)]

simmat <- read.csv("data/similaritymatrixdc.csv",check.names=FALSE)
rownames(simmat) <- colnames(simmat)
list_shows <- sort(unique(vwc$choice_name))
colnames(given) <- unique(vwd$sets)
rownames(given) <- colnames(given)
given <- given[,order(colnames(given))]
given <- given[order(rownames(given)),]

list_clusters <- function(simmat,song,clust,clustassign){
  
  l <- names(clustassign[[clust]][which(clustassign[[clust]] == clustassign[[clust]][which(names(clustassign[[clust]]) == song)])])
  
  l <- l[-which(l==song)]
  nrow = 6
  ncol <- ceiling(length(l) / nrow)
  pad  <- nrow * ncol - length(l)
  l <- as.data.table(matrix(c(l, rep("", pad)), nrow = nrow, byrow = TRUE))
  
  #l <- as.data.table(matrix(l, nrow = 6, byrow = TRUE))
  

  
  return(l)
}

plot_clusters <- function(kmeans_result,simmat,clust){
  fviz_cluster(kmeans_results[clust][[1]], data = simmat, 
               ellipse.type = "convex", 
               ggtheme = theme_bw()
  )}

get_next_desc <- function(song,song_next_per){
  return(song_next_per[[song]])
}

get_streamgraph <- function(album_percentages_nf){
  ggplot(album_percentages_nf, aes(x = date_num, y = percentage, fill = album)) +
    geom_stream( bw =0.6,color = 1, lwd = 0.3)+
    scale_fill_manual(values = album_col) +
    theme_void()
}

get_linegraph <- function(album_percentages_nf){
  ggplot(album_percentages_nf, aes(x = date_num, y = percentage, group = album, col = album)) +
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
}

get_piechart <- function(shows,setlist_albums_songs){
  if(length(shows)>0){
  city_songs <- setlist_albums_songs %>%
    filter(
      choice_name %in% shows
    ) %>%
    select(
      album, sets, col
    )
  
  sum_songs <- city_songs %>%
    group_by(
      album
    ) %>%
    mutate(
      n_album = n(),
      album_per = round(n_album / nrow(city_songs),2)
    ) %>%
    ungroup() %>%
    group_by(sets) %>%
    mutate(
      num_song = n()
    ) %>%
    ungroup() %>%
    mutate(
      a_label = paste(album,", ",album_per*100,"%",sep = ""),
      s_label = paste(sets,", ",num_song,sep = "")
    )
  
  a <- sum_songs$album
  a_p <- sum_songs$a_label
  s_n <- sum_songs$s_label
  b <- sum_songs$sets
  c<- sum_songs$num_song
  
  l<- rbind(unique(cbind(a,a_p)),cbind(b,s_n))
  
  p <- c(rep("",length(unique(a))),a)
  
  num_albums <- unique(cbind(sum_songs$album,sum_songs$n_album))[,2]
  
  v <- as.numeric(c(num_albums,sum_songs$num_song))
  
  i <- c(unique(a),paste(a,"-",b,sep = ""))
  
  a <- na.omit(as.data.frame(c(l[1:length(unique(a))],p[length(unique(a))+1:length(p)])))
  colnames(a) <- "album"
  
  c <- left_join(a,albumcolors,by="album")
  
  h <- paste("<b>",l,"</b><br>",ifelse(l %in% a,paste(a_p*100,"%<br>",a_n," times"),""))
  
  argh <- unique(as.data.frame(cbind(l,p,v,i,c)))
  
  argh <- arrange(argh,desc(v))
  
  fig1 <- plot_ly(argh)
  
  fig1 <- fig1 %>% add_trace(
    labels = ~a,
    parents = ~p,
    values = ~v,
    type = 'sunburst',
    insidetextorientation='radial',
    branchvalues='total',
    hovertemplate = ~a_p
  ) %>%
    layout(
      extendsunburstcolors = TRUE,
      colorway = ~col)
  
  fig1
  }
  
}

get_heatmap <- function(given){
  colnames(given) <- str_trunc(colnames(given),12)
  
  rownames(given) <- str_trunc(rownames(given),12)
  hm <- pheatmap(given,
           color = colorRampPalette(c("white", "blue", "darkblue"))(50), # Gradient colors
           display_numbers = TRUE, # Show similarity values
           number_color = "black") 
  return(hm)# Number color
}

percent_given <- function(given,song1,song2){
  return(round(given[which(colnames(given) == song1),which(colnames(given) == song2)],2))
}

all_percent_given <- function(given,song){
  return(cbind(rownames(given),t(round(given[which(colnames(given) == song),],2))))
}

# get_setlist <- function(choice,song_show_wide) {
#   
#   song_show_wide %>%
#     select(
#       all_of(choice)
#     )%>%
#     as.data.frame() %>%
#     return()
#   
#   #return(unname(unlist(song_show_wide[show])))
#   
# }

get_setlist <- function(choice, song_show_wide) {
  choice <- intersect(choice, names(song_show_wide))
  if (length(choice) == 0) return(data.frame())
  maxlen <- max(sapply(choice, function(nm) length(song_show_wide[[nm]][[1]])))
  cols <- lapply(choice, function(nm) {
    x <- song_show_wide[[nm]][[1]]
    length(x) <- maxlen         # pad with NA to equal length
    x
  })
  df <- as.data.frame(setNames(cols, choice), check.names = FALSE)
  return(df)
}

get_song_totalsVW <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  VW <- na.omit(piv_total[c("song","Vampire Weekend")])
  VW_gt <- VW %>% gt() %>%
    data_color(
      columns = 'Vampire Weekend',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)+1))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  return(VW_gt)
}

get_song_totalsC <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
 
  C <- na.omit(piv_total[c("song","Contra")])
  
  
  C_gt <- C %>% gt() %>%
    data_color(
      columns = 'Contra',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  return(C_gt)}

get_song_totalsM <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  MVOTC <- na.omit(piv_total[c("song","Modern Vampires of the City")])

  
  MVOTC_gt <- MVOTC %>% gt() %>%
    data_color(
      columns = 'Modern Vampires of the City',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  
  return(MVOTC_gt)}

get_song_totalsF <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )
  
  FOTB <- na.omit(piv_total[c("song","Father of the Bride")])

  

  FOTB_gt <- FOTB %>% gt() %>%
    data_color(
      columns = 'Father of the Bride',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  return(FOTB_gt)}

get_song_totalsO <- function(shows,vwc,albums) {
  show_songs <- vwc[c("sets","album")][vwc$choice_name %in% shows,]
  
  names(show_songs) <- c("song","album")
  
  freq <- show_songs %>%
    group_by(song)%>%
    summarize(
      freq_song = n()
    ) 
  
  total_songs <- albums %>%
    left_join(freq,by = "song") %>%
    mutate(
      freq_song = ifelse(is.na(freq_song),0,freq_song))
  
  piv_total <- pivot_wider(
    total_songs, names_from = album,
    values_from = freq_song
  )

  OGWAU <- na.omit(piv_total[c("song","Only God Was Above Us")])
  
  OGWAU_gt <- OGWAU %>% gt() %>%
    data_color(
      columns = 'Only God Was Above Us',
      target_columns = everything(),
      colors = col_numeric(c("white", "orange"), domain = c(0,length(shows)))
    )%>%
    tab_options(
      table.font.size = 12
    )
  
  return(OGWAU_gt)}


# get_dag <- function(shows,setlists){
#   
#   setlists_1 <- setlists[which(shows %in% names(setlists))]
#   
#   song_positions <- do.call(rbind, lapply(setlists_1, function(setlist) {
#     data.frame(Song = setlist, Position = seq_along(setlist))
#   }))
#   
#   avg_positions <- aggregate(Position ~ Song, data = song_positions, mean)
#   rownames(avg_positions) <- avg_positions$Song 
#   
#   
#   edges <- unlist(lapply(setlists_1, function(setlist) {
#     if (length(setlist) > 1) {
#       transitions <- cbind(setlist[-length(setlist)], setlist[-1])
#       return(as.vector(t(transitions))) 
#     }
#   }), use.names = FALSE)
#   
#   
#   edges_with_shows <- do.call(rbind, lapply(names(setlists_1), function(show) {
#     setlist <- setlists[[show]]
#     if (length(setlist) > 1) {
#       transitions <- data.frame(
#         From = setlist[-length(setlist)],
#         To = setlist[-1],
#         Show = show
#       )
#       return(transitions)
#     }
#   }))
#   
#   starting_songs <- unique(sapply(setlists_1, function(setlist) setlist[1]))
#   
#   graph <- graph_from_data_frame(edges_with_shows, directed = TRUE)
#   
#   vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)
#   
#   layout <- layout_as_tree(graph) 
#   y_positions <- -avg_positions[V(graph)$name, "Position"]
#   
#   layout <- cbind(layout[, 1], y_positions)
#   
#   show_colors <- setNames(sample(rainbow(length(setlists_1))), names(setlists_1)) 
#   
#   
#   edge_colors <- show_colors[edges_with_shows$Show]
#   
#   plot(
#     graph,
#     layout = layout,
#     vertex.label.font = vertex_font_weight,
#     vertex.label.color = "black",
#     edge.arrow.size = 0.2,
#     edge.color = edge_colors,
#     vertex.shape = "none",
#     vertex.label.cex=.8,
#     edge.curved=.2,
#     main = "Setlist Directed Graph",
#     asp = 1.5
#   )
#   
#   
# }

get_dag <- function(shows,vwc){
  
  songlist <- vwc[,c("choice_name","sets","song_num")]
  
  setlists_1 <- songlist[which(songlist$choice_name %in% shows),]
  
  setlists_1 <- setlists_1 %>%
    group_by(choice_name) %>%
    mutate(song_num = row_number()) %>%
    ungroup()
  
  song_pos_avg <- setlists_1 %>%
    select(
      sets, song_num
    ) %>%
    group_by(
      sets
    ) %>%
    summarize(
      avg_pos = max(song_num)
    ) %>%
    mutate(
      name = sets
    )
  
  edges <- data.frame(From = NA, To = setlists_1$sets[1], Show = setlists_1$choice_name[1])
  
  for(i in 2:(nrow(setlists_1)-1)){
    edges <- rbind(edges,data.frame(From = setlists_1$sets[i-1], To = setlists_1$sets[i], Show = setlists_1$choice_name[i]))
  }
  
  for(show in shows){
    edges <- edges[-min(which(edges$Show == show)),]
  }
  
  edges <- na.omit(edges)
  
  
  starting_songs <- setlists_1$sets[setlists_1$song_num==1]
  
  graph <- graph_from_data_frame(edges, directed = TRUE)
  
  vertex_font_weight <- ifelse(V(graph)$name %in% starting_songs, 2, 1)
  
  show_colors <- setNames(sample(rainbow(length(shows))), sort(unique(setlists_1$choice_name)))
  
  
  edge_colors <- show_colors[edges$Show]
  
  plot(
    graph,
    layout = layout_with_sugiyama,,
    vertex.label.font = vertex_font_weight,
    vertex.label.color = "black",
    edge.arrow.size = 0.2,
    edge.color = edge_colors,
    vertex.shape = "rectangle",
    vertex.label.cex=.8,
    vertex.color = "white",
    vertex.frame.color = "white",
    main = "Setlist Directed Graph",
    asp = 1.5,
    arrow.mode = 0
  )
  legend("topright",bty = "n",
         legend=unique(names(edge_colors)),
         fill=unique(edge_colors), border=NA,
         cex = 0.7)
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navset_tab(
    
#####

nav_panel("Home",
          
          titlePanel("VW OGWAU Tour Statistics"),
          
          h5("I was super bored and missing the OGWAU tour so here are some statistics on it. I do not claim to be smart or knowledgeable in statistics even though I got a degree in it.")
          
          
          
          ),

#####
nav_panel("Setlists",

          titlePanel("Setlists"),

          sidebarLayout(

            sidebarPanel(

              h5("Choose which shows you went to and you will be shown the setlists."),
              checkboxGroupInput("showssetlist", "Shows:", list_shows,selected = "Amsterdam: 2024-12-15")
            ),
            mainPanel(
              DTOutput("settable")

            )


          )
),

nav_panel("Song Totals",
          
          titlePanel("Song Totals"),
          
          sidebarLayout(
            
            sidebarPanel(
              
              h5("Choose which shows you went to and you will be shown what songs you've seen."),
              checkboxGroupInput("setshows", "Shows:", list_shows,selected = "Amsterdam: 2024-12-15"),
              width = 2
            ),
            mainPanel(
              fluidRow( # First row for the first table
                column(2, # Occupy 6 out of 12 columns (half width)
                       gt_output("albumtableVW")
                ),
                column(2, # Occupy the remaining 6 columns
                       gt_output("albumtableC")
                )
                ,
                column(2, # Occupy the remaining 6 columns
                       gt_output("albumtableM")
                       )
                ,
                  column(2,
                         gt_output("albumtableF")
                         ),
                  column(2,
                         gt_output("albumtableO"))
                )
              
            )
            
            
          )
),



    
#####
    nav_panel("Clusters",
    

    titlePanel("KMeans Clustering"),

 
    sidebarLayout(
        sidebarPanel(
          
            h5("K-means clustering is a method of grouping similar items into clusters. I used it to analyze which songs appeared together in setlists. If a song was played, the algorithm identifies other songs that were most likely also played."),
            h6("(If you choose a song that is basically always played like Ice Cream Piano, then you will get the other songs that were also basically always played, so I recommend choosing some \"deeper cuts\"!)"),  
          
            sliderInput("k",
                        "Number of clusters:",
                        min = 2,
                        max = 10,
                        value = 2),
            selectInput("ksong", "Choose a song to see the cluster",
                        choices = colnames(simmat))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("kPlot"),
           textOutput("ktext"),
           tableOutput("clusttable")
        )
    )
  ),
#####
  
  nav_panel("Next Song",
            
      titlePanel("Which Song is Next?"),
      sidebarLayout(
      sidebarPanel(
            
            selectInput("nsong", "Choose a song to see what songs usually come next",
                        choices = colnames(simmat))
      ),
      mainPanel(
        tableOutput("nextsong")
      )
            )
  ),
  
#####
  nav_panel("Streamgraph",
            
            titlePanel("Streamgraph"),
            
            sidebarLayout(
              sidebarPanel(
                
                h5("This is a streamgraph of how many songs from each album they played at each show over time. You can see that they were pretty consistent. Note that festivals were taken out.")
                
              ),
              
              mainPanel(
                plotOutput("sgplot")
              )
              
            )
        
            ),

nav_panel("Albums over time",
          
          titlePanel("Albums over time"),
          
          sidebarLayout(
            sidebarPanel(
              
              h5("This is a line graph of how many songs from each album they played at each show over time. You can see that they were pretty consistent, but Contra did go up near the end. Loess smoothing was used. Note that festivals were taken out.")
              
            ),
            
            mainPanel(
              plotOutput("lineplot")
            )
            
          )
          
),
  
  
#####
nav_panel("Album Percentages",

          titlePanel("What Album Percent Breakdown Did You See?"),
          sidebarLayout(

            sidebarPanel(
              h5("Check which shows you went to, and the pie chart will update to show you how many songs from each album they played, and the overall percentage."),
            checkboxGroupInput("cbperc", "Shows:", list_shows,selected = "Amsterdam")
            ),
            mainPanel(

              plotlyOutput("percplot")

            )
          )

          ),

#####
nav_panel("Two Songs",
          
          titlePanel("Given X, What Percent of the Time Did They Play Y?"),
          
          sidebarLayout(
            
            sidebarPanel(
              h5("Choose two songs. This will give you the percent of time that the second song was played given they played the first song."),
              h6("(If you choose a song that always played, such as Ice Cream Piano, you will get back the overall percentage of the time that song was played throughout the tour.)"),
              selectInput("fsong", "First Song",
                          choices = colnames(simmat)),
              selectInput("ssong", "Second Song",
                          choices = colnames(simmat)),
              checkboxInput("allsong","Check this box if you want to see all song percentages given the first song")
            ),
            mainPanel(
              tableOutput("fstable")
            )
            
            
          )
          
          ),
#####
nav_panel("Heatmap",
          
          titlePanel("Heatmap of Song Occurances"),
          
              h5("This is a big graph. I know. The higher the number, the more the two songs were played together. Given the song on the row played, this is the percentage of the time the song on the column played."),
          h6("(Sorry about the order of the songs, it is based on a clustering algorithm.)"),
           
            
              plotOutput("heatmap", height = "900px")
            
          
          
          ),

#####

nav_panel("Dendrogram",
          
          titlePanel("Dendrogram of Songs"),
          
              h5("A dendrogram is a tree-like chart that shows how items are grouped based on their similarity. In my analysis, I used the order of songs in each setlist to group them into clusters. Each of the 6 groups I created represents an \"act\" in the setlist, with songs in the same group being likely to appear together during a show."),
              
              imageOutput("dend")
          
          
          ),


#####
nav_panel("Overlap",
          
          titlePanel("Overlap of Shows Map"),
          
          h5("This map shows relatively how common your setlist was at your show. If it's blue, there might have been some deeper cuts (see the other map). If it's red, you must have gotten a lot of the hits!"),
          h5("I took away the festivals, as they were all extremely close to each other, with them only playing their really popular songs. It would have skewed the data."),
          
          leafletOutput("oleaf")
          
          ),

#####
nav_panel("Deep Cuts",
          
          titlePanel("Deep Cuts Map"),
          
          h5("This map has your show on it if VW played a song that they played less than 5 times for the entire tour. The shows in gold highlight places where they played a song only one time. That song is indicated by an asterisk."),
          
          leafletOutput("dleaf")
          
),

#####
nav_panel("Order of Songs",
          
          titlePanel("Directed Graph of Setlists"),
          sidebarLayout(
            
            sidebarPanel(
              h5("Check which shows you went to, and the graph will update to show you what order you saw each song at. Bold songs were played first. Comparing two setlists works great, three and up is where it gets hard to read.  The arrows are sometimes flipped, I don't know why."),
              checkboxGroupInput("dagshows", "Shows:", list_shows)
            ),
            mainPanel(
              
              plotOutput("dagplot")
              
            )
          )
          
)
)
)



#####


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$kPlot <- renderPlot({
      
      plot_clusters(kmeans_results,simmat,input$k)
      
    })
    
    output$ktext <- renderText({paste(input$ksong,":", sep = "")})
  
    output$clusttable <- renderTable({
      
      list_clusters(simmat,input$ksong,input$k,clustassign)
      
    }, colnames = FALSE, bordered = TRUE)
    
    output$nextsong <- renderTable({
      get_next_desc(input$nsong,song_next_per)
    }, colnames = FALSE)
    
    
    output$sgplot <- renderPlot({
      
      get_streamgraph(album_percentages)
      
    })
    output$lineplot <- renderPlot({
      
      get_linegraph(album_percentages_nf)
      
    })
    
    output$percplot <- renderPlotly({
      get_piechart(input$cbperc,setlist_albums_songs)
    })
    
    output$fstable <- renderTable({
      if(input$allsong){
        g <- as.data.frame(all_percent_given(given,input$fsong))
        colnames(g) <- c("V1","V2")
        g <- g[order(g$V2,decreasing = TRUE),]
        colnames(g) <- c("Song",paste("Percent played given",input$fsong,"played"))
      }
      else{
        g <- percent_given(given,input$fsong,input$ssong)
        g <- as.data.frame(c(paste("Given ",input$fsong," played, ", input$ssong," played ",g[1]*100,"% of the time.", sep = "")))
        names(g) <- ""
      }
      g
    })
    
    output$heatmap <- renderPlot({
      get_heatmap(given)
    })
    
    output$dend <- renderImage(
      {
        list(src = "data/dendrogram.png", alt = "Dendrogram", width = "1000px")}
      
      , deleteFile = FALSE
    )
    
    # output$settable <- renderTable(
    #   
    #   {get_setlist(input$setshows,song_show_wide)}
    # )
    
    # output$settable <- renderTable({
    #   req(input$setshows)
    #   get_setlist(input$setshows, song_show_wide)
    # }, na = "", striped = TRUE, bordered = TRUE, spacing = "xs")
    
    output$settable <- DT::renderDT({
      req(input$showssetlist)
      
      df <- get_setlist(input$showssetlist, song_show_wide)
      
      present_songs <- sort(unique(na.omit(unlist(df, use.names = FALSE))))
      
      song_col_present <- l_song_col[song_col$song %in% present_songs,]

      dt <- datatable(df, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
        formatStyle(
          input$showssetlist,
          backgroundColor = styleEqual(song_col_present$song, song_col_present$col)
        )
      dt}
    )
    
    output$albumtableVW <- render_gt(
      {get_song_totalsVW(input$setshows,vwc,albums)}
    )
    
    output$albumtableC <- render_gt(
      {get_song_totalsC(input$setshows,vwc,albums)}
    )
    
    output$albumtableM <- render_gt(
      {get_song_totalsM(input$setshows,vwc,albums)}
    )
    
    output$albumtableF <- render_gt(
      {get_song_totalsF(input$setshows,vwc,albums)}
    )
    
    output$albumtableO <- render_gt(
      {get_song_totalsO(input$setshows,vwc,albums)}
    )
    
    output$oleaf <- renderLeaflet({
      leaflet(showavgoco) %>% 
        addTiles()  %>% 
        setView( lat=36, lng=-44 , zoom=2) %>%
        addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken='4szQK8qDpn6FyJKGDjggzDvo9O9qRmgWRrKRtre2nWgqJ5PFPaBcztuNUFPPM2SW')) %>%
        addCircleMarkers(~lon, ~lat, 
                         fillColor = ~mypalette(showavgo), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~showavgo, opacity=1, title = "Overlap Percentage </br> Average: 0.57", position = "bottomright" )
    })
    
    output$dleaf <- renderLeaflet({
      leaflet(show_sets) %>% 
        addTiles()  %>% 
        setView( lat=36, lng=-44 , zoom=2) %>%
        addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken='4szQK8qDpn6FyJKGDjggzDvo9O9qRmgWRrKRtre2nWgqJ5PFPaBcztuNUFPPM2SW')) %>%
        addCircleMarkers(~lon, ~lat, 
                         fillColor = ~color,fillOpacity = 0.7, color="white", radius=6, stroke=T,
                         label = mytextdc,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        )
    })
    
    output$dagplot <- renderPlot({
      
      get_dag(input$dagshows,vwc)
      },
      height = 1000
    )
      
    }


# Run the application 
shinyApp(ui = ui, server = server)

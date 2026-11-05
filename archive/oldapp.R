nav_panel("Albums over time",
          
          titlePanel("Albums over time"),
          
          sidebarLayout(
            sidebarPanel(
              
              h5("These are a line graph and stream graph of how many songs from each album they played at each show over time. You can see that they were pretty consistent, but Contra did go up near the end. Loess smoothing was used for the line graph. Note that festivals were taken out.")
              
            ),
            
            mainPanel(
              plotOutput("lineplot"),
              plotOutput("sgplot")
            )
            
          )
          
),


##############



nav_panel(
  "Albums Over Time",
  
  bslib::layout_columns(
    # ---- LEFT: side panel ----
    bslib::card(
      bslib::card_header("Albums Over Time"),
      h5("These are a line graph and stream graph of how many songs from each album they played at each show over time. You can see that they were pretty consistent, but Contra did go up near the end. Loess smoothing was used for the line graph. Note that festivals were taken out."),
    
    # ---- RIGHT: two stacked cards ----
    tags$div(
      # a tiny gap between the two cards
      style = "display: flex; flex-direction: column; gap: 1rem;",
      
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Stream Graph"),
        plotOutput("sgplot", height = "55vh")
      ),
      
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Line Graph"),
        plotOutput("lineplot", height = "55vh")
      )
    ),
    
    col_widths = c(3, 9)
  )
)
,
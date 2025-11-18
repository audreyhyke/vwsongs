library(markovchain)
library(tidyverse)

load("/vwshiny/data/vwc_pt2.RData")

vwc_o <- vwc %>%
  mutate(
    sets_o = ifelse(song_num %in% c(1,2,3) & sets %in% c("Campus","Cape Cod Kwassa Kwassa","Cousins","Diane Young","M79", "Oxford Comma","Unbelievers"), paste0(sets," (opener)"),sets)
  )


vwc_o %>%
  filter(
    sets == "Cape Cod Kwassa Kwassa",
    festival == "VW Show"
  ) %>%
  mutate(
    opens = ifelse(song_num== 1, "opener", ifelse(song_num %in% c(2,3,4), "after opener before curtain", "after curtain"))
  ) %>%
  select(
    opens
  ) %>%
  table()%>%
  as.data.frame() %>%
  mutate(
    per = paste0(round(Freq/sum(Freq),2)*100,"%")
  )


vwc %>%
  filter(
    sets == "Cousins",
    song_num == 1
  ) %>%
  nrow()

vwc_o %>%
  filter(
    sets == "Holiday"
  ) %>%
  select(
    sets_o
  ) %>%
  table()




cousins_neighbors <- vwc_o %>%
  filter(
    sets_o == "Cousins"
  ) %>%
  select(
    date, song_num
  )

c_n_s <- list()

for(i in 1:nrow(cousins_neighbors)){
  s_num <- cousins_neighbors[i,2][[1]]
  s_nums <- c(s_num-1,s_num,s_num+1)
  songs <- vwc %>%
    filter(
      date == as.Date(cousins_neighbors[i,1][[1]]),
      song_num %in% s_nums
    ) %>%
    select(
      sets
    ) %>%
    paste0()
  c_n_s <- append(c_n_s,songs)
}

c_n_s %>% unlist() %>% table()

dy_n <- vwc_o %>%
  group_by(date)%>%
  filter(
    "Cousins (opener)" %in% sets_o
  ) %>%
  ungroup()%>%
  filter(
    sets == "Diane Young"
  ) %>%
  select(
    date, song_num
  )

after_dy <- list()

for(i in 1:nrow(dy_n)){
  after_dy_s <- vwc %>%
    filter(
      date == as.Date(dy_n[i,1][[1]]),
      song_num == dy_n[i,2][[1]] + 1
    ) %>%
    select(
      sets
    )
  after_dy <- append(after_dy, after_dy_s)
}
  
after_dy %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    per = paste0(round(Freq/sum(Freq),2)*100,"%")
  ) %>%
  arrange(desc(per))
  


openers_dist <- vwc %>%
  group_by(city_num) %>%
  slice_min(song_num, with_ties = FALSE) %>%
  ungroup() %>%
  count(sets, name = "n") %>%
  mutate(p = n / sum(n)) %>%
  select(sets, p)


setlists <- vwc %>%
  filter(
    festival == "VW Show"
  ) %>%
  group_by(city_num) %>%
  arrange(
    song_num
  ) %>%
  select(
    sets
  ) %>%
  pivot_wider(
    names_from = city_num,
    values_from = sets
  )



mcsets <- list()



for(show in setlists) {
  show <- show[[1]]

  if("Cousins" %in% show){
    i <- which(show=="Cousins")
    if(i < 4){
      show[i] <- "Cousins (opener)"
    }
  }
  if("Unbelievers" %in% show){
    i <- which(show=="Unbelievers")
    if(i < 4){
      show[i] <- "Unbelievers (opener)"
    }
  }
  if("Cape Cod Kwassa Kwassa" %in% show){
    i <- which(show=="Cape Cod Kwassa Kwassa")
    if(i < 4){
      show[i] <- "Cape Cod Kwassa Kwassa (opener)"
    }
  }
  if("M79" %in% show){
    i <- which(show=="M79")
    if(i < 4){
      show[i] <- "M79 (opener)"
    }
  }
  
  if("Campus" %in% show){
    i <- which(show=="Campus")
    if(i < 4){
      show[i] <- "Campus (opener)"
    }
  }
  
  if("Oxford Comma" %in% show){
    i <- which(show=="Oxford Comma")
    if(i < 4){
      show[i] <- "Oxford Comma (opener)"
    }
  }
  
  if("Diane Young" %in% show){
    i <- which(show=="Diane Young")
    if(i < 4){
      show[i] <- "Diane Young (opener)"
    }
  }
  
  mcsets <- append(mcsets,list(show))
}

len_dist <- length(mcsets)

fit <- markovchainFit(data = mcsets, method = "mle", name = "SetlistChain", byrow = TRUE)
chain <- fit$estimate  

simulate_set <- function(chain,
                         target_len = NULL,
                         start_song = NULL,
                         allow_repeat = FALSE,
                         len_dist = NULL,
                         openers_dist = NULL,
                         closers = NULL) {
  
  states <- chain@states
  TM <- chain@transitionMatrix
  
  # choose set length
  if (is.null(target_len)) {
    if (is.null(len_dist)) stop("Provide len_dist or target_len.")
    target_len <- sample(len_dist, 1)
  }
  
  # choose starting song
  if (is.null(start_song)) {
    if (is.null(openers_dist)) {
      start_song <- sample(states, 1)                 # fallback: uniform
    } else {
      pool <- intersect(openers_dist$sets, states)
      probs <- openers_dist$p[match(pool, openers_dist$sets)]
      probs <- probs / sum(probs)
      start_song <- sample(pool, 1, prob = probs)
    }
  } else if (!(start_song %in% states)) {
    stop("start_song not found in chain states.")
  }
  
  set <- c(start_song)
  
  while (length(set) < target_len) {
    cur <- tail(set, 1)
    
    # stop if a designated closer was hit
    if (!is.null(closers) && cur %in% closers) break
    
    probs <- as.numeric(TM[cur, ])
    names(probs) <- states
    
    # forbid repeats, if desired
    if (!allow_repeat) {
      probs[names(probs) %in% set] <- 0
    }
    
    if (sum(probs) <= 0) break           # dead end / absorbing → stop early
    probs <- probs / sum(probs)
    
    nxt <- sample(states, 1, prob = probs)
    set <- c(set, nxt)
  }
  
  set
}

simulate_many <- function(n_sets = 10,
                          chain,
                          len_dist,
                          start_song = NULL,
                          allow_repeat = FALSE,
                          openers_dist = NULL,
                          closers = NULL) {
  sims <- map(seq_len(n_sets), ~simulate_set(chain,
                                             target_len = NULL,
                                             start_song = start_song,
                                             allow_repeat = allow_repeat,
                                             len_dist = len_dist,
                                             openers_dist = openers_dist,
                                             closers = closers))
  tibble(sim_id = seq_along(sims),
         song = sims) %>%
    unnest_longer(song, indices_include = TRUE, values_to = "sets") %>%
    rename(song_num = sets_id)
}
  
sim_mansard <- simulate_many(n_sets = 5,
                             chain = chain,
                             len_dist = len_dist,
                             start_song = "Mansard Roof",
                             allow_repeat = FALSE)

closers <- c("Walcott", "Ya Hey")  # <- adjust to your anchors
sim_with_closer <- simulate_many(n_sets = 5,
                                 chain = chain,
                                 len_dist = len_dist,
                                 start_song = "Mansard Roof",
                                 closers = closers)
#################
#### percent occurance given songs played????

song_choice <- c("Sunflower","Hannah Hunt")
song_choice <- c("Holiday","Ice Cream Piano","Hope")

setlists_o <- list()

for(show in setlists){
  show <- show[[1]]
  if("Cousins" %in% show){
    i <- which(show=="Cousins")
    if(i < 4){
      show[i] <- "Cousins (opener)"
    }
  }
  if("Cape Cod Kwassa Kwassa" %in% show){
    i <- which(show=="Cape Cod Kwassa Kwassa")
    if(i < 4){
      show[i] <- "Cape Cod Kwassa Kwassa (opener)"
    }
  }
  if("M79" %in% show){
    i <- which(show=="M79")
    if(i < 4){
      show[i] <- "M79 (opener)"
    }
  }
  
  if("Campus" %in% show){
    i <- which(show=="Campus")
    if(i < 4){
      show[i] <- "Campus (opener)"
    }
  }
  
  if("Oxford Comma" %in% show){
    i <- which(show=="Oxford Comma")
    if(i < 4){
      show[i] <- "Oxford Comma (opener)"
    }
  }
  if("Unbelievers" %in% show){
    i <- which(show=="Unbelievers")
    if(i < 4){
      show[i] <- "Unbelievers (opener)"
    }
  }
  
  if("Diane Young" %in% show){
    i <- which(show=="Diane Young")
    if(i < 4){
      show[i] <- "Diane Young (opener)"
    }
  }
  setlists_o <- append(setlists_o,list(show))
}

names(setlists_o) <- names(setlists)


scsets <- list()
for(show in setlists_o){
  if(all(song_choice %in% show)){
    scsets<- append(scsets,list(show))
  }
}

scsetsp <- scsets %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    perc = ifelse(Freq/length(scsets) > 1, 1, Freq/length(scsets))
  ) %>%
  arrange(
    desc(perc)
  ) %>%
  rename(
    sets = "."
  )

openers <- setlists_o %>%
  map(1) %>%
  unlist() %>%
  unname() %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    p = Freq/sum(Freq)
  )

sets_p <- vwc_o %>%
  select(sets) %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    p = Freq/length(unique(vwc_o$city_num))
  ) %>%
  mutate(
    p = ifelse(p > 1, 1, p)
  )

n_nf <- vwc %>% 
  filter(festival == "VW Show")%>%
  select(city_num) %>%
  unique() %>%
  nrow()
  

sets_p_nf <- vwc_o %>%
  filter(festival=="VW Show") %>%
  select(sets_o) %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    p = Freq/n_nf
  ) %>%
  mutate(
    p = ifelse(p > 1, 1, p)
  ) %>%
  rename(
    sets = sets_o
  )

set_p_dif <- scsetsp %>%
  left_join(
    sets_p_nf, by = "sets"
  ) %>%
  mutate(
    dif_r = (perc-p)/p,
    dif_s = dif_r * sqrt(Freq.y)
  ) %>%
  rename(
    p_all = p,
    p_chosen = perc,
    f_chosen = Freq.x,
    f_all = Freq.y
  ) %>%
  filter(
    !(sets %in% song_choice)
  ) %>%
  arrange(
    desc(abs(dif_s))
  ) %>%
  head()
  

song_choice <- c("New Dorp. New York")

scsets <- list()
for(show in setlists_o){
  if(all(song_choice %in% show)){
    scsets<- append(scsets,list(show))
  }
}

scsetsp <- scsets %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  mutate(
    perc = ifelse(Freq/length(scsets) > 1, 1, Freq/length(scsets))
  ) %>%
  arrange(
    desc(perc)
  ) %>%
  rename(
    sets = "."
  )

set_p_dif <- scsetsp %>%
  left_join(
    sets_p_nf, by = "sets"
  ) %>%
  mutate(
    dif_r = (perc-p)/p,
    dif_s = dif_r * sqrt(Freq.y)
  ) %>%
  rename(
    p_all = p,
    p_chosen = perc,
    f_chosen = Freq.x,
    f_all = Freq.y
  ) 

set_p_dif_h <- set_p_dif%>%
  filter(
    !(sets %in% song_choice),
    f_all >3
  ) %>%
  arrange(
    desc(abs(dif_s))
  ) %>%
  head()

setdiff(sets_p_nf$sets[sets_p_nf$p>.5],set_p_dif$sets )

song_choice <- "Hannah Hunt"


scsets <- list()
for(show in setlists_o){
  if(all(song_choice %in% show)){
    scsets<- append(scsets,list(show))
  }
}

avg_length <- scsets %>%
  map(length) %>%
  unlist() %>%
  mean() %>%
  ceiling()

core_songs <- scsets %>%
  Reduce(intersect,.)

b4ICP <- list()
for(sets in scsets){
  i <- which(sets=="Ice Cream Piano")
  print(i)
  b4ICP <- append(b4ICP,list(sets[0:(i-1)]))
}

avg_b4ICP_length <- b4ICP %>%
  map(length) %>%
  unlist() %>%
  mean() %>%
  ceiling()

openers_b4ICP <- b4ICP %>%
  map(1) %>%
  unlist() %>%
  table()

opener_b4ICP <- openers_b4ICP %>% 
  which.max() %>%
  names()
  

avgs_b4ICP <- b4ICP %>%
  unlist() %>%
  table() %>%
  as.data.frame()


openers_b4ICP_2 <- tibble(song = names(openers_b4ICP),
                      open_freq = as.numeric(openers_b4ICP))

b4ICP_no <- avgs_b4ICP %>%
  rename(song = ".") %>%
  left_join(openers_b4ICP_2, by = "song") %>%
  mutate(freq_o = Freq - coalesce(open_freq, 0)) %>%
  select(
    song, freq_o
  ) %>%
  arrange(desc(freq_o)) %>%
  slice(1:(avg_b4ICP_length-1))


second_b4ICP <- b4ICP %>%
  map(2) %>%
  unlist() %>%
  table() %>%
  which.max() %>%
  names()

third_b4ICP <- b4ICP %>%
  map(3) %>%
  unlist() %>%
  table() %>%
  which.max() %>%
  names()










#############

song_choice <- "New Dorp. New York"

scsets <- list()
for(show in setlists_o){
  if(all(song_choice %in% show)){
    scsets<- append(scsets,list(show))
  }
}

avg_length <- scsets %>%
  map(length) %>%
  unlist() %>%
  mean() %>%
  ceiling()

core_songs <- scsets %>%
  Reduce(intersect,.)

avg_core <- map_dfr(seq_along(scsets), function(i) {
  tibble(
    show_id = i,
    song = scsets[[i]],
    song_num = seq_along(scsets[[i]])
  )
}) %>%
  filter(song %in% core_songs) %>%
  group_by(song) %>%
  summarise(avg_position = floor(mean(song_num, na.rm = TRUE)))

avg_set <- rep(NA,avg_length)
for(i in 1:avg_length){
  if(i %in% avg_core$avg_position){
    avg_set[i] <- avg_core$song[which(avg_core$avg_position == i)]
  }
  else{
  avg_set[i] <- scsets %>%
    map(i) %>%
    unlist() %>%
    table() %>%
    which.max() %>%
    names() %>%
    sample(1)
  }
}

########

avg_core <- map_dfr(seq_along(scsets), function(i) {
  tibble(
    show_id = i,
    song = scsets[[i]],
    song_num = seq_along(scsets[[i]])
  )
}) %>%
  filter(song %in% core_songs) %>%
  group_by(song) %>%
  summarise(avg_position = mean(song_num, na.rm = TRUE)) %>%
  arrange(avg_position)

avg_setlist <- list()
i = 1
for(c in 1:(length(core_songs)-1)){
  currents <- core_songs[c]
  nexts <- core_songs[c+1]
  
  core_pos <- round(avg_core$avg_position[avg_core$song == currents])
  core_pos <- ifelse(core_pos<i,i,core_pos)
  l_btwn <- core_pos-i
  
  print(
    paste0("C:",currents,"  N:",nexts,"  CP:",core_pos,"  I:",i,"  L:",l_btwn)
  )
  if(l_btwn > 0){
    songs_btwn <- list()
    for(sets in scsets){
      cur <- F
      nex <- F
      s_list <- list()
      for(s in sets){
        if(s == currents){cur <- T}
        if(s == nexts){nex <- T}
        if(cur & !nex){s_list <- append(s_list,s)}
      }
      s_list <- s_list[-1]
      songs_btwn <- append(songs_btwn,s_list)
    }
    set_btwn <- rep(NA,l_btwn)
  for(s in 1:l_btwn){
    set_btwn[s] <- scsets %>%
      map(s) %>%
      unlist() %>%
      table() %>%
      which.max() %>%
      names() %>%
      sample(1)
  }
  
  avg_setlist <- append(avg_setlist,set_btwn,currents)
  
  }
  
  i = core_pos + 1
  
}


#######
## TRY AGAIN

scsetsse <- list()

# add "start" and "end"
for(i in 1:length(scsets)){
  scsetsse <- append(scsetsse,list(append(append("start",scsets[[i]]),"end")))
}

scsetsse <- map(scsets, ~ c("start", .x, "end"))

core_songs <- scsetsse %>%
  Reduce(intersect,.)


avg_set <- list()

for(i in 1:(length(core_songs)-1)){
  
  othersongs <- list()
  
  othersongs <- map(scsetsse, ~ {
    i1 <- which(.x == core_songs[i])
    i2 <- which(.x == core_songs[i + 1])
    if (length(i1) == 0 || length(i2) == 0 || i1 > i2) return(NA_character_)
    .x[(i1+1):(i2-1)]
  })
  
  avg_length <- othersongs %>%
    map(length) %>%
    unlist() %>%
    mean() %>%
    round()
  avg_run <- rep(NA, avg_length)
  for(ind in 1:length(othersongs)){
  avg_run <- othersongs %>%
    map(i) %>%/;;;;;;;;;;;;;;[']]]]]]]]]]]]p']
    unlist() %>%
    table() %>%
    which.max() %>%
    names() %>%
    sample(1)
  }
  
}



vwc %>%
  filter(
    festival != "VW Show",
    date != "2024-09-21",
    sets == "White Sky"
  ) %>%
  select(
    festival, venue, date 
  )
  




















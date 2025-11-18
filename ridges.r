library(ggridges)
library(hdrcde)

set_totals <- vwc %>%
  group_by(choice_name) %>%
  mutate(song_num = row_number()) %>%
  ungroup() %>%
  select(sets,song_num) %>%
  group_by(
    sets
  ) %>%
  summarize(
    set_total = n()
  ) %>%
  filter(
    set_total > 10
  ) %>%
  select(
    sets
  )

anchors <- vwc %>%
  group_by(choice_name) %>%
  mutate(song_num = row_number()) %>%
  ungroup() %>%
  select(sets,song_num) %>%
  group_by(
    sets
  ) %>%
  summarize(
    set_total = n()
  ) %>%
  filter(
    set_total > 70
  ) %>%
  select(
    sets
  )

sets_sn <- vwc %>%
  filter(festival == "VW Show") %>%
  group_by(choice_name) %>%
  mutate(song_num = row_number()) %>%
  ungroup() %>%
  select(sets,song_num) %>%
  filter(
    sets %in% set_totals$sets
  ) 

sets_01 <- vwc %>%
  filter(festival == "VW Show") %>%
  group_by(choice_name) %>%
  mutate(song_num = row_number(),
         song_num01 = song_num/max(song_num)) %>%
  ungroup() %>%
  select(
    sets,song_num01
  ) %>%
  filter(sets %in% set_totals$sets)

sets_avg01 <- sets_01 %>%
  group_by(sets) %>%
  summarize(
    song_dens_avg = density(song_num01)$x[which.max(density(song_num01)$y)],
    song_avg = mean(song_num01)
  )

sets_avg <- sets_sn %>%
  group_by(sets) %>%
  summarize(
    song_dens_avg = density(song_num)$x[which.max(density(song_num)$y)],
    song_avg = mean(song_num)
  )

sets_sn <- sets_sn %>%
  left_join(
    sets_avg, by = "sets"
  ) %>%
  mutate(
    sets = reorder(sets, -song_dens_avg)   # order by average position
  ) %>%
  mutate(
    anchor = ifelse(sets %in% anchors$sets,"> 70","<= 70")
  )


sets_sn01 <- sets_sn %>%
  left_join(
    sets_avg01, by = "sets"
  ) %>%
  mutate(
    sets = reorder(sets, -song_dens_avg)   # order by average position
  ) %>%
  mutate(
    anchor = ifelse(sets %in% anchors$sets,"> 70","<= 70")
  )

sets_sn01 <- cbind(sets_sn01,sets_01$song_num01)
names(sets_sn01) <- c(names(sets_sn01)[1:5],"song_num01")


ggplot(sets_sn01,aes(x = song_num01, y = sets, fill = anchor)) +
  geom_density_ridges(
    alpha = 0.75,
    linewidth = 0.3
  ) +
  theme_ridges()+
  xlab("Scaled Song Position") +
  ylab("")+
  scale_fill_manual(
    values = c("black","grey"),
    limits = c("> 70","<= 70")
  )

ggplot(sets_sn,aes(x = song_num, y = sets)) +
  geom_segment(
    data = distinct(sets_sn, sets),
    aes(x = -10, xend = 37, y = sets, yend = sets),
    color = "white", linewidth = 0.45
  ) +
  geom_density_ridges(
    fill = "black",
    scale = 5,
    bandwidth = 0.76,
    col = "white",
    linewidth = 0.4
  ) +
  theme_void() +
  xlim(-10,40) + 
  theme(plot.background = element_rect(fill = "black")) 

  








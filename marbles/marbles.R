library(tidyverse)
library(gganimate)
library(ggtext)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

marbles_plot <- marbles %>% 
  filter(!str_detect(race, "Q")) %>% # remove qualification rounds
  select(race, team_name, points) %>% 
  group_by(team_name) %>% 
  mutate(cumpoints = cumsum(points),
         race = str_remove(race, "S1R") %>% as.numeric) %>% # turn race into a number
  group_by(race) %>% 
  mutate(rank = rank(-cumpoints, ties.method = "first"), # ties.method = "first": every team has a different rank, default is "average"
         value_label = paste0(" ", cumpoints)) %>%
  ggplot(., aes(rank, group = team_name)) +
  geom_tile(aes(y = cumpoints/2,
                height = cumpoints,
                width = .85),
            alpha = .65, color = NA) +
  geom_text(aes(y = 0, 
                label = paste(team_name, "  ")), 
            vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(y = cumpoints,
                label = value_label, hjust = 0),
            size = 8 ) +
  coord_flip(clip = "off", expand = T) +
  scale_x_reverse() +
  transition_states(race) +
  labs(title = "Result after race# {closest_state}",
       caption = "@adrianclo1\nsource: Jelle's Marble Runs") +
  theme(plot.title = element_text(hjust = 1, size = 22),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y  = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "gray"),
        plot.margin = margin(1, 1, 1, 6, "cm")) +
  ease_aes("sine-in-out")

animate(marbles_plot, fps = 7, start_pause = 10, renderer = gifski_renderer("marbles_plot.gif"))

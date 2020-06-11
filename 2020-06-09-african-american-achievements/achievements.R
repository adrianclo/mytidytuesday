library(tidyverse)
library(tidytuesdayR)

retrieve_slope <- function(x) { return(x$coeff[2]) }

tuesdata <- tidytuesdayR::tt_load("2020-06-09")
firsts <- tuesdata$firsts

# 1865: 13th amendment: emancipation
# 1954: start civil rights movement
# 1964: 24th amendment

# include a dataset for "overall"
df <- firsts %>% 
  count(year) %>% 
  mutate(cum_n = cumsum(n),
         category = "Overall") %>%
  bind_rows(
    firsts %>% 
      count(category, year) %>% 
      group_by(category) %>% 
      mutate(cum_n = cumsum(n))  
  ) %>% 
  mutate(category = factor(category, levels = c("Overall", "Arts & Entertainment",
                                                "Education & Science", "Law", 
                                                "Military", "Politics", 
                                                "Religion", "Social & Jobs", 
                                                "Sports")),
         # highlight periods
         period = case_when(year < 1865 ~ "before emancipation",
                            between(year,1865,1954) ~ "after emancipation",
                            year > 1954 ~ "after civil rights movement"))

# create individual points per category + overall
ind_points <- firsts %>%
  mutate(period = NA,
         cum_n = 0) %>% 
  bind_rows(
    firsts %>%
      mutate(category = "Overall",
             period = NA,
             cum_n = 0)
  ) %>% 
  mutate(category = factor(category, levels = c("Overall", "Arts & Entertainment",
                                                "Education & Science", "Law",
                                                "Military", "Politics",
                                                "Religion", "Social & Jobs",
                                                "Sports")))

ggplot(df, aes(year, cum_n, group = period)) +
  geom_vline(xintercept = c(1865,1954), linetype = "dashed") +
  geom_point(alpha = .1, data = ind_points) +
  geom_smooth(se = F, method = "lm", color = "gray", alpha = .05, linetype = "dashed") +
  geom_path(aes(color = period), show.legend = F, size = 1) +
  facet_wrap(~category, scales = "free_y") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1700,2020, 40)) +
  labs(x = "Year", y = "Cumulative number of achievements",
       title = "Rate of achievements for African Americans increases by >200% after\nthe emancipation (1865), and the start of the Civil Rights Movement (1954)",
       subtitle = "Exception: Rate of achievements in {Religion} between the two milestones is increased by 30%",
       caption = "Data: Wikipedia\nViz by: adrianclo1") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(size = 20), plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 16)) 

ggsave("achievements.png")

df %>% 
  nest(data = -c(category, period)) %>% 
  mutate(model = map(data, ~lm(cum_n ~ year, data = .x)),
         slope = map_dbl(model, ~retrieve_slope(.x))) %>% 
  select(category, period, slope) %>% 
  group_by(category) %>% 
  mutate(slope_next = lead(slope)) %>% 
  mutate(slope_change = (slope_next / slope)*100) %>% 
  as.data.frame()


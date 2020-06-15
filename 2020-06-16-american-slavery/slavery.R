library(tidyverse)
library(tidytuesdayR)

tuesdata <- tt_load('2020-06-16')
tuesdata

census <- tuesdata$census
names <- tuesdata$african_names
routes <- tuesdata$slave_routes
blackpast <- tuesdata$blackpast

View(census)
View(names)
View(routes)

routes %>% 
  filter(voyage_id == 2314)

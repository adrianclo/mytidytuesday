library(tidyverse)
library(tidytuesdayR)

tuesdata <- tt_load("2020-06-16")
tuesdata

census <- tuesdata$census
names <- tuesdata$african_names
routes <- tuesdata$slave_routes
blackpast <- tuesdata$blackpast

View(census)
View(names)
View(routes)
View(blackpast)

routes %>% 
  filter(voyage_id == 81711)
names %>% 
  filter(voyage_id == 81711)
names %>% 
  filter(voyage_id == 81740)

routes %>% 
  count(port_origin, sort = T)

routes %>% 
  count(ship_name, sort = T) %>% 
  View()

routes %>% 
  count(n_slaves_arrived, sort = T)

routes %>% dim()
routes %>% 
  filter(is.na(n_slaves_arrived))

routes %>% 
  count(port_origin, port_arrival, sort = T) %>% 
  as.data.frame()

census %>% 
  count(region, division)

names %>% 
  count(gender)

routes %>% 
  nest(data = -c(ship_name)) %>% 
  mutate(NAs = map_dbl(data, ~sum(is.na(.x$n_slaves_arrived))),
         prop_NAs = map2_dbl(NAs, data, ~.x/nrow(.y)),
         voyages = map_dbl(data, ~nrow(.x))) %>% 
  arrange(desc(voyages))

routes %>% 
  nest(data = -c(ship_name)) %>% 
  mutate(NAs = map_dbl(data, ~sum(is.na(.x$n_slaves_arrived))),
         prop_NAs = map2_dbl(NAs, data, ~.x/nrow(.y)),
         voyages = map_dbl(data, ~nrow(.x))) %>% 
  arrange(desc(prop_NAs))

routes %>% 
  nest(data = -c(ship_name)) %>% 
  mutate(NAs = map_dbl(data, ~sum(is.na(.x$n_slaves_arrived))),
         prop_NAs = map2_dbl(NAs, data, ~.x/nrow(.y)),
         voyages = map_dbl(data, ~nrow(.x))) %>% 
  arrange(prop_NAs)

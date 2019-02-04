############### Extracting PoI from OSM in R ############################################
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(leaflet)
library(dplyr)

# osm PoI's: zie ook https://wiki.openstreetmap.org/wiki/Map_Features

#### Querying PoI's #####################################################################

# Cheese and cannabis shops 
out =  getbb("Amsterdam")%>%
  opq() %>%
  add_osm_feature("shop") %>% 
  osmdata_sf()

# data
out2 = out$osm_points

# look at cheese and cannabis shops
cheese_cannabis =  out2 %>% 
  select(name, addr.city, opening_hours, shop ) %>%
  mutate(shop = as.character(shop)) %>% 
  filter(shop %in% c("cheese", "cannabis"))

# put them on a leaflet map
ccIcons <- iconList(
  cheese = makeIcon("cheese.png",  25, 25),
  cannabis = makeIcon("cannabis.png", 25, 25)
)

leaflet(cheese_cannabis) %>%
  addTiles() %>%
  addMarkers(icon = ~ccIcons[shop], popup = ~name)









## Een gewone ggmap kaartje
ggmap(amsterdam_map) +
  geom_sf(data = cheesewinkels,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha = .95,
          size = 2,
          shape=21)+
  labs(title = "cheese shops in Amsterdam")

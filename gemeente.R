library(leaflet)
library(leafgl)
library(maptools)
library(cleangeo)
library(rgdal)
library(sf)

#### inlezen shape file
NL_gemeente =  sf::st_read("2019_gemeentegrenzen_kustlijn.gpkg")

#### Zet coordinatensysteem
NL_gemeente = sf::st_transform(NL_gemeente, "+proj=longlat +datum=WGS84")
NL_gem = st_cast(NL_gemeente, "POLYGON")

### traditional plot
plot(NL_gem)


### leaflet plot
M = leaflet() %>%
  addTiles() %>% 
  addPolygons(data = NL_gem, label =NL_gem$gemeentenaam )


M

library(leaflet)
library(leafgl)
library(maptools)
library(cleangeo)
library(rgdal)
library(sf)

#### inlezen shape file
#NL_gemeente =  sf::st_read("2019_gemeentegrenzen_kustlijn.gpkg")
NL_gemeente =  sf::st_read("ESRI-PC4-2017R1.shp")

#### Zet coordinatensysteem
NL_gemeente = sf::st_transform(NL_gemeente, "+proj=longlat +datum=WGS84")
NL_gem = st_cast(NL_gemeente, "POLYGON")

### traditional plot
plot(NL_gem)


colpal <- colorQuantile(
  palette = colorRamps::green2red(7), n=7,
  domain = NL_gem$Aantal_adr 
)

### leaflet plot
M = leaflet() %>%
  addTiles() %>% 
  addPolygons(
    data = NL_gem,  
    weight = 1, 
    label = NL_gem$PC4,  
    fillColor = colpal( NL_gem$Aantal_adr )
  )
M



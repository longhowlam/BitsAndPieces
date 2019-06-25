library(leaflet)
library(stringr)
library(dplyr)
library(sf)

#### inlezen shape file en restaurant data
PC4_shapes =  sf::st_read("ESRI-PC4-2017R1.shp")
Restaurants = readr::read_csv("Restaurants.csv")

#### Zet coordinatensysteem en make polygon
PC4_shapes = PC4_shapes %>% 
  sf::st_transform("+proj=longlat +datum=WGS84") %>% 
  st_cast("POLYGON")

### traditional R plot
plot(PC4_shapes)


### leaflet plot with colors

colpal <- colorQuantile(
  palette = colorRamps::green2red(7), n=7,
  domain = PC4_shapes$Aantal_adr 
)


M = leaflet() %>%
  addTiles() %>% 
  addPolygons(
    data = PC4_shapes,  
    weight = 1, 
    label = PC4_shapes$PC4,  
    fillColor = colpal( PC4_shapes$Aantal_adr )
  )
M

### leaflet plot with restaurant density

PC4_rest = Restaurants %>% 
  mutate(PC4 = str_sub(PCs,1,4)) %>% 
  group_by(PC4) %>% 
  summarise(N_restaurants = n())

PC4_shapes2 = PC4_shapes %>% 
  left_join(PC4_rest) %>% 
  mutate(
    restaurant_perc = N_restaurants / (Aantal_adr/1000)
  )

hist(PC4_shapes2$restaurant_perc)

pal <- colorQuantile(
  palette = colorRamps::green2red(13), n=13,
  domain =  PC4_shapes2$restaurant_perc
)



M = leaflet(PC4_shapes2) %>%
  addTiles() %>% 
  addPolygons(
    opacity = 0.125,
    fillOpacity = 0.5,
    weight = 1, 
    label = paste(
      PC4_shapes2$PC4, 
      round(PC4_shapes2$restaurant_perc)
    ),
    fillColor = ~pal(restaurant_perc)
  ) %>% 
  addLegend("bottomright", pal = pal, values = ~restaurant_perc,
                           opacity = 1
  )
M






library(osmdata)
out =  getbb("drente")%>%
  opq() %>%
  add_osm_feature("amenity") %>% 
  osmdata_sf()

# data
out2 = out$osm_points %>% filter(amenity == "restaurant")

# look at cheese and cannabis shops
cheese_cannabis =  out2 %>% 
  select(name, addr.city, opening_hours, shop ) %>%
  mutate(shop = as.character(shop)) %>% 
  filter(shop %in% c("cheese", "cannabis"))





















#### Get Restaurants in Noord Holland from OSM data
NH =  getbb("Noord Holland")%>%
  opq(timeout = 60) %>%
  add_osm_feature("amenity") %>% 
  osmdata_sf() %>% 
  .$osm_points %>% 
  filter(amenity == "restaurant") %>% 
  as.data.frame() %>% 
  select(addr.city, name, geometry)

#### Read in Posctode 4 position shape files: https://www.imergis.nl/htm/opendata.htm
## and put in right coordinate system
PC4_shapes =  sf::st_read("ESRI-PC4-2017R1.shp") %>% 
  sf::st_transform("+proj=longlat +datum=WGS84") %>% 
  st_cast("POLYGON")  

###### We need postcode 4 of each restaurant we can get that by determining
## in which PC4 shape the geompoint of the restaurant  is located

pcindex = st_intersects(NH$geometry, PC4_shapes$geometry)
NH$PC4 = PC4_shapes$PC4[pcindex %>% unlist]

#### group restaurants on PC4 level
PC4_rest = NH %>% 
  group_by(PC4) %>% 
  summarise(N_restaurants = n())

#### join with shapes data
PC4_shapes2 = PC4_shapes %>% 
  left_join(PC4_rest) %>% 
  mutate(
    restaurant_perc = N_restaurants / (Aantal_adr/1000)
  )

### define color scales and put shapes on leaflet map
pal <- colorQuantile(
  palette = colorRamps::green2red(13), n=13,
  domain =  PC4_shapes2$restaurant_perc
)

leaflet(PC4_shapes2) %>%
  addTiles() %>% 
  addPolygons(
    opacity = 0.125,
    fillOpacity = 0.5,
    weight = 1, 
    label = paste("Postcode 4:",PC4_shapes2$PC4, 
      "<br/> N rest", PC4_shapes2$N_restaurants, "N adresses", PC4_shapes2$Aantal_adr
    ),
    fillColor = ~pal(restaurant_perc)
  )


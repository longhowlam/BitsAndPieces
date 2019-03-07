library(tidyverse)
library(circlepackeR)
library(data.tree)
library(treemap)

## import eggs data
eggs <- read_csv("aaj1945_DataS1_Egg_shape_by_species.csv")

## summarize on Order and family
eggs.sum = eggs %>% 
  group_by(Order, Family) %>% 
  summarise(
    n=n(),
    avg_length = mean(`AvgLength (cm)`),
    avg_elipticity = mean(Ellipticity)
  ) %>% 
  mutate(
    webpath = paste("All", Order, Family, sep = "/")
  )

## transform this data to a hierarchical tree that is suitable for circlepackR
eggs.hr = as.Node(eggs.sum, pathName = "webpath")
circlepackeR(eggs.hr, size = "avg_length", width = '800px', height = '800px')
circlepackeR(eggs.hr, size = "avg_elipticity", width = '800px', height = '800px')

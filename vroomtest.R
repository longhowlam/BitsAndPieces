library(vroom)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


### Import data ####################################################################################

gebreken = vroom(
  "/home/ruser/data/rdw_gebreken.csv",
  .name_repair = janitor::make_clean_names, 
  col_types = list(`Meld datum door keuringsinstantie` = col_date("%Y%m%d"))
)

gebreken_code = vroom(
  "/home/ruser/data/gebreken_code.csv",
  .name_repair = janitor::make_clean_names
)

autos = vroom(
  "/home/ruser/data/rdw_voertuigen.csv",
  .name_repair = janitor::make_clean_names
)

### prepare data data ####################################################################################

top_gebreken_perperiode = gebreken %>%
  group_by(
    datum = floor_date(meld_datum_door_keuringsinstantie, "day"),
    gebrek_identificatie
  ) %>% 
  summarise(
    n=n()
  ) %>% 
  group_by(datum) %>% 
  mutate(
    rank = rank(desc(n), ties.method = "first")
  ) %>% 
  left_join(gebreken_code) 
  

### Plot data ########################################################################################

top_gebreken_perperiode %>%
  filter(
    rank <= 10,
    datum >= ymd("20170501"),
    datum < ymd("20190501")
  ) %>% 
  ggplot(
    aes(datum,  n, color = gebrek_omschrijving )
  ) + 
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "month", date_labels = "%m-%y") +
  labs(title = "Twee jaar RDW data van mei 2017 tot mei 2019 \nTop 10 aantal gemelde gebreken aan auto's ")



library(dplyr)
library(ggplot2)
library(readr)

# data can be downloaded from here:
# https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf

# import data
covid <- read_csv("COVID-19_Case_Surveillance_Public_Use_Data.csv")

# create 0/1 variables
covid = covid %>%
  mutate(
    hosp = case_when(
      hosp_yn == "Yes" ~  1,
      hosp_yn == "No" ~ 0
    ),
    icu = case_when(
      icu_yn == "Yes" ~  1,
      icu_yn == "No" ~ 0
    ),
    death = case_when(
      death_yn == "Yes" ~  1,
      death_yn == "No" ~ 0
    )
    
  )

## aggregate per group
out = covid %>% 
  filter(sex %in% c("Female", "Male")) %>% 
  filter(age_group !="Unknown") %>% 
  filter( medcond_yn !="Unknown") %>% 
  group_by(sex,  age_group, medcond_yn) %>% 
  summarise(
    n = n(),
    mortality_rate = mean(death, na.rm = TRUE)
  ) 
  


ggplot(out, aes(fill=medcond_yn, y=mortality_rate, x=age_group)) +
  geom_bar(position="dodge", stat="identity") + facet_grid(~sex) +
  coord_flip() +
  scale_y_continuous(name="mortality rate", limits=c(0, 0.74), breaks = (0:10)/10) 


## logistic regression 
lrdata = covid %>% 
  filter(sex %in% c("Female", "Male")) %>% 
  filter(age_group !="Unknown") %>% 
  filter( medcond_yn =="No") %>% 
  filter( death_yn !="Unknown") %>% 
  mutate(Y = as.factor(death_yn), age = as.factor(age_group))

lr = glm( Y ~ age, data = lrdata, family=binomial())
summary(lr)
